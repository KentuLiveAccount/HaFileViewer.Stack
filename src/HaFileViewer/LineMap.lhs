Sparse Line Map for Gigabyte-Scale Text File Viewing
=====================================================

This module provides efficient line-oriented access to very large text files
through lazy index building and sparse caching.

Design Goals
------------

1. **Lazy Population**: Index builds on-demand as regions are accessed
2. **Memory Efficiency**: Only cache byte offsets at K-line intervals (default 1024)
3. **Fast Random Access**: O(1) lookup for indexed regions, O(n/K) for new regions
4. **Stateful Caching**: Use IORef to share growing index across calls
5. **UTF-8 Support**: Handle multi-byte encodings and chunk boundaries correctly

Architecture Overview
--------------------

The LineMap maintains two key pieces of state as IORef value:
- `lmForward`: Sparse index of byte offsets (0, K, 2K, ...)
- `lmIndexed`: Tracks how far we've scanned

When accessing line N:
1. Find nearest cached offset <= N from sparse index
2. Scan forward from that offset, building index as we go
3. Return requested lines and cache any new index entries

This approach balances memory usage with access speed for typical file viewer
navigation patterns (sequential browsing, jumping, searching).

Line Ending Support
-------------------

The implementation handles the most common line ending conventions:

- **Unix/Linux (LF)**: `0x0A` - Fully supported (primary delimiter)
- **Windows (CRLF)**: `0x0D 0x0A` - Fully supported (CR stripped after split)
- **Classic Mac (CR only)**: `0x0D` - NOT supported (rare, pre-OSX format)

Files are split on LF byte (`0x0A`), then trailing CR bytes are removed from
each line via `normalizeLine`. This handles both Unix and Windows files correctly.

Classic Mac CR-only files (pre-OSX) would appear as a single long line. Adding
support would require more complex splitting logic and CRLF boundary handling.
This is considered acceptable as CR-only files are extremely rare in modern use.

Module Header
-------------

> {-# LANGUAGE OverloadedStrings #-}
> module HaFileViewer.LineMap
>   ( LineMap
>   , openLineMap
>   , closeLineMap
>   , getLines
>   , indexStepDefault
>   ) where
>
> import qualified Data.Vector.Storable as VS
> import qualified Data.ByteString as BS
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as TE
> import qualified Data.Text.Encoding.Error as TEE
> import System.IO.MMap (mmapFileByteString)
> import System.IO (withFile, IOMode(..), hFileSize)
> import Data.Word
> import Data.IORef
> import Control.Monad (when)
> import Data.Int (Int64)

Constants
---------

Byte value for line feed (newline) character.

> lfByte :: Word8
> lfByte = 10

Core Data Types
---------------

The LineMap encapsulates both the file handle and the lazily-built index.
Using IORef allows the index to grow transparently as the file is accessed,
without requiring callers to thread state through every operation.

LineMap is intentionally kept opaque to the user of the module.

> type Offset = Integer
>
> data LineMap = LineMap
>   { lmPath        :: FilePath
>   , lmFileSize    :: Integer
>   , lmWindowSize  :: Integer
>   , lmWindow      :: IORef (Offset, BS.ByteString)  -- (start offset, mapped data)
>   , lmIndexStep   :: Int
>   , lmForward     :: IORef (VS.Vector Word64) -- byte offsets at lines 0, K, 2K, ...
>   , lmIndexed     :: IORef Integer -- how many lines have been scanned from start
>   }

The default index step of 1024 provides good balance:
- Memory: ~8 bytes per 1024 lines = 8KB per million lines
- Speed: Maximum 1024-line scan to reach any target

indexStepDefault :: Int
------------------
Recommended default value for the index step parameter.

This constant defines how many lines apart index checkpoints are placed.
Larger values use less memory but require more scanning; smaller values
use more memory but enable faster seeks.

> indexStepDefault :: Int
> indexStepDefault = 1024

windowSizeDefault :: Integer
------------------
Recommended default window size for memory-mapped region.

For gigabyte-scale files, mapping the entire file wastes address space.
Instead, we map a sliding window and remap as needed when accessing
different regions.

100MB provides good balance:
- Large enough to avoid frequent remapping during sequential access
- Small enough to keep memory usage bounded for huge files
- Multiple windows can fit in typical RAM for concurrent file access

> windowSizeDefault :: Integer
> windowSizeDefault = 100 * 1024 * 1024  -- 100 MB

Public API
----------

The API is deliberately minimal to support the two primary use cases:
1. Web server: cache LineMap and serve line ranges to clients
2. CUI app: hold LineMap during navigation session

Opening a file initializes the sparse index with just offset 0 (start of file).
All other index entries build lazily as regions are accessed.

openLineMap :: FilePath -> Int -> IO LineMap
-------------
Open a file for line-oriented reading with sparse index.

Parameters:
  path :: FilePath
    - Path to the text file to open
    - File must exist and be readable
    - File is opened in binary mode for precise byte offset control
  
  k :: Int
    - Index step size (lines between index checkpoints)
    - Use 'indexStepDefault' (1024) for typical cases
    - Larger values (e.g., 4096) save memory for huge files
    - Smaller values (e.g., 256) speed up seeks for frequent random access

Returns:
  LineMap handle with initial index containing only offset 0
  Call 'closeLineMap' when done to release file handle

> openLineMap :: FilePath -> Int -> IO LineMap
> openLineMap path k = do
>   -- Get file size
>   fileSize <- withFile path ReadMode (fmap fromIntegral . hFileSize)
>   -- Map initial window starting at offset 0
>   let winSize = min windowSizeDefault fileSize
>       mapSize = if winSize > 0 then Just (0, fromIntegral winSize) else Nothing
>   initialWindow <- if fileSize > 0
>                    then mmapFileByteString path mapSize
>                    else return BS.empty
>   winRef <- newIORef (0, initialWindow)
>   fref <- newIORef (VS.singleton 0) -- offset 0 at line 0
>   iref <- newIORef 0
>   return $ LineMap path fileSize windowSizeDefault winRef k fref iref

closeLineMap :: LineMap -> IO ()
-------------
Close the file handle associated with a LineMap.

Parameters:
  lm :: LineMap
    - The LineMap to close
    - After calling this, the LineMap must not be used again
    - The sparse index data is discarded (not persisted)

Note: Consider using 'bracket' or similar resource management to ensure
the file is closed even if exceptions occur.

> closeLineMap :: LineMap -> IO ()
> closeLineMap lm = return ()  -- mmap cleanup handled automatically by GC

Line Access with Negative Indexing
-----------------------------------

The primary operation returns a slice of lines starting at a given position.
Negative indices count from end-of-file (Python-style).

Design decision: When negative indexing is used, we perform a full scan to
count total lines. This is acceptable because:
1. Negative indexing is rare (mostly "jump to end")
2. The scan result could be cached if needed
3. Alternative would require backward index building (complex)

getLines :: LineMap -> Integer -> Int -> IO [Text]
---------
Read a slice of lines from the file.

Parameters:
  lm :: LineMap
    - The LineMap handle from 'openLineMap'
    - Index builds lazily as needed when accessing new regions
  
  start :: Integer
    - Zero-based line number to start reading from
    - Positive: line offset from beginning (0 = first line)
    - Negative: line offset from end (-1 = last line, -2 = second-to-last)
    - Out-of-bounds values return empty list
  
  count :: Int
    - Maximum number of lines to return
    - If fewer lines available, returns what's available
    - Use 0 to return empty list

Returns:
  List of Text lines (without trailing newlines)
  - Lines have CR characters stripped (handles Windows line endings)
  - UTF-8 decoded leniently (invalid sequences replaced, not errors)
  - Empty list if start is beyond EOF or count is 0

Examples:
  getLines lm 0 10      -- First 10 lines
  getLines lm 100 5     -- Lines 100-104
  getLines lm (-5) 5    -- Last 5 lines
  getLines lm (-1) 1    -- Last line only

> getLines :: LineMap -> Integer -> Int -> IO [T.Text]
> getLines lm start count = do
>   -- Translate negative start to positive by computing total lines (simple scan)
>   startPos <- if start >= 0
>     then return start
>     else do
>       total <- countTotalLines lm
>       return $ max 0 (total + start)
>   -- find nearest indexed line <= startPos
>   let k = fromIntegral (lmIndexStep lm)
>   let baseLine = (startPos `div` k) * k
>   baseOffset <- findOrScanTo lm baseLine
>   -- now scan forward from baseOffset until we reach startPos + count
>   lns <- scanLinesFromOffset lm baseOffset baseLine startPos count
>   return lns

Helper: Windowed Memory Mapping
-------------------------------

Ensures that a region of the file is mapped into memory. If the requested
offset is outside the current window, remaps a new window centered around it.

The window is sized to minimize remapping while keeping memory bounded.

> ensureMapped :: LineMap -> Offset -> Integer -> IO BS.ByteString
> ensureMapped lm offset size = do
>   (winStart, winData) <- readIORef (lmWindow lm)
>   let winEnd = winStart + fromIntegral (BS.length winData)
>       needsRemap = offset < winStart || offset + size > winEnd
>   
>   if needsRemap
>     then do
>       -- Center the new window around the requested offset
>       let halfWin = lmWindowSize lm `div` 2
>           newStart = max 0 (offset - halfWin)
>           maxSize = lmFileSize lm - newStart
>           newSize = min (lmWindowSize lm) maxSize
>           mapSpec = if newSize > 0 
>                     then Just (fromIntegral newStart, fromIntegral newSize)
>                     else Nothing
>       newData <- if newSize > 0
>                  then mmapFileByteString (lmPath lm) mapSpec
>                  else return BS.empty
>       writeIORef (lmWindow lm) (newStart, newData)
>       return newData
>     else return winData

Helper: Count Total Lines
--------------------------

Scan file counting lines using memory-mapped content.
A line is text terminated by LF or EOF.

Note: This counts actual lines, not just newlines. A file ending without
a trailing newline still has its last line counted.

For large files, this scans in windows to avoid mapping the entire file.

> countTotalLines :: LineMap -> IO Integer
> countTotalLines lm = do
>   let fileSize = lmFileSize lm
>   if fileSize == 0
>     then return 0  -- Empty file has 0 lines
>     else do
>       -- Scan file in windows, counting newlines
>       let winSize = lmWindowSize lm
>           loop offset nlCount = do
>             if offset >= fileSize
>               then return nlCount
>               else do
>                 let remaining = fileSize - offset
>                     chunkSize = min winSize remaining
>                 _ <- ensureMapped lm offset chunkSize
>                 (winStart, winData) <- readIORef (lmWindow lm)
>                 let relOffset = offset - winStart
>                     chunk = BS.take (fromIntegral chunkSize) $ BS.drop (fromIntegral relOffset) winData
>                     count = fromIntegral $ BS.count lfByte chunk
>                 loop (offset + chunkSize) (nlCount + count)
>       nlCount <- loop 0 0
>       -- Check if file ends with newline
>       _ <- ensureMapped lm (fileSize - 1) 1
>       (winStart, winData) <- readIORef (lmWindow lm)
>       let lastByte = BS.index winData (fromIntegral (fileSize - 1 - winStart))
>           endsWithNL = lastByte == lfByte
>           lineCount = if endsWithNL then nlCount else nlCount + 1
>       return lineCount

Index Lookup and Building
--------------------------

This is the core of the lazy index strategy:
1. Check if baseLine is already in the sparse index
2. If yes, return cached offset (O(1))
3. If no, scan from last known index to build up to baseLine

The index grows incrementally, caching offsets at every K-line boundary
encountered during scans.

> findOrScanTo :: LineMap -> Integer -> IO Offset
> findOrScanTo lm baseLine = do
>   if baseLine == 0 then return 0
>   else do
>     fwd <- readIORef (lmForward lm)
>     let k = fromIntegral (lmIndexStep lm)
>         targetIdx = fromIntegral (baseLine `div` k)
>         lastIdx = VS.length fwd - 1
>     
>     if targetIdx <= lastIdx
>       then return $ fromIntegral (fwd VS.! targetIdx)
>       else do
>         -- Need to scan forward from last known index to build up to targetIdx
>         let startLine = fromIntegral lastIdx * k
>             startOffset = fromIntegral (fwd VS.! lastIdx)
>         _ <- scanAndBuildIndex lm startOffset startLine baseLine
>         -- After building, check if target was cached
>         fwd' <- readIORef (lmForward lm)
>         let lastIdx' = VS.length fwd' - 1
>         if targetIdx <= lastIdx'
>           then return $ fromIntegral (fwd' VS.! targetIdx)
>           else return $ fromIntegral (fwd' VS.! lastIdx')  -- Return closest we have

Index Building Strategy
-----------------------

When scanning forward, we read chunks from the mapped file and count newlines.
Whenever we cross a K-line boundary, we precisely locate that boundary's byte
offset and cache it.

This "scan and record crossings" approach ensures:
- Index remains accurate (exact byte offsets)
- No redundant work (we're scanning anyway to reach the target)
- Index density matches access pattern (frequently accessed regions get indexed)

> scanAndBuildIndex :: LineMap -> Offset -> Integer -> Integer -> IO Offset
> scanAndBuildIndex lm startOffset startLine targetLine = do
>   let fileSize = lmFileSize lm
>       k = fromIntegral (lmIndexStep lm)
>       buf = 65536
>       loop offset curLine = do
>         if curLine >= targetLine
>           then return offset
>           else do
>             let remaining = fileSize - offset
>             if remaining <= 0
>               then return offset
>               else do
>                 let chunkSize = min buf remaining
>                 _ <- ensureMapped lm offset chunkSize
>                 (winStart, winData) <- readIORef (lmWindow lm)
>                 let relOffset = offset - winStart
>                     chunk = BS.take (fromIntegral chunkSize) $ BS.drop (fromIntegral relOffset) winData
>                     newlines = fromIntegral $ BS.count lfByte chunk
>                     newOffset = offset + fromIntegral (BS.length chunk)
>                 -- Check if we crossed any index boundaries and record them
>                 let oldIdx = curLine `div` k
>                     newLine = curLine + newlines
>                     newIdx = newLine `div` k
>                 when (newIdx > oldIdx) $ do
>                   -- We crossed index boundaries, need to find exact offsets
>                   recordIndexCrossings lm offset curLine chunk k (oldIdx + 1) newIdx
>                 loop newOffset newLine
>   loop startOffset startLine

Recording Index Crossings
--------------------------

When a chunk scan crosses one or more K-line boundaries, we need to find the
exact byte offset of each boundary. This requires a finer-grained scan through
the chunk bytes.

Design note: We do this immediately rather than deferring because:
1. Chunk data is hot in cache
2. Deferred recording would complicate state management
3. Index precision matters for performance

> recordIndexCrossings :: LineMap -> Offset -> Integer -> BS.ByteString -> Integer -> Integer -> Integer -> IO ()
> recordIndexCrossings lm baseOffset baseLine chunk k fromIdx toIdx = do
>   when (fromIdx <= toIdx) $ do
>     let targetLine = fromIdx * k
>     -- Scan through chunk to find exact offset for targetLine
>     offset <- findLineOffset baseOffset baseLine chunk targetLine
>     -- Append to forward index
>     modifyIORef' (lmForward lm) (\v -> VS.snoc v (fromIntegral offset))
>     modifyIORef' (lmIndexed lm) (max targetLine)
>     -- Recursively record next index
>     recordIndexCrossings lm baseOffset baseLine chunk k (fromIdx + 1) toIdx

Precise Offset Location
------------------------

Given a chunk and a target line number, find the exact byte offset where
that line starts. This is a simple byte-by-byte scan counting newlines.

> findLineOffset :: Offset -> Integer -> BS.ByteString -> Integer -> IO Offset
> findLineOffset baseOffset baseLine chunk targetLine = do
>   let bytes = BS.unpack chunk
>       findOff off _ [] = return off
>       findOff off line (b:bs)
>         | line >= targetLine = return off
>         | b == lfByte = findOff (off + 1) (line + 1) bs
>         | otherwise = findOff (off + 1) line bs
>   findOff baseOffset baseLine bytes

Line Extraction with Chunk Boundaries
--------------------------------------

The most complex part: read lines from a byte offset, handling:
1. UTF-8 multi-byte sequences split across chunks
2. Lines longer than chunk size (partial line carry-over)
3. Files ending without trailing newline
4. Efficient slice extraction (skip lines before startLine)

Key insight: Split chunks on LF, track whether chunk ended with LF,
and carry partial fragments to next chunk if needed.

> scanLinesFromOffset :: LineMap -> Offset -> Integer -> Integer -> Int -> IO [T.Text]
> scanLinesFromOffset lm off baseLine startLine count = do
>   let fileSize = lmFileSize lm
>       chunkSize = 65536
>       go curLine acc remaining partial offset = do
>         if remaining <= 0 then return acc
>         else do
>           let bytesRemaining = fileSize - offset
>           if bytesRemaining <= 0
>             then do -- EOF: include partial only if it falls within requested window
>               let includePartial = (not (BS.null partial)) && (curLine >= startLine) && (remaining > 0)
>                   acc' = if includePartial then acc ++ [decodeUtf8Lenient partial] else acc
>               return acc'
>             else do
>               let readSize = min chunkSize bytesRemaining
>               _ <- ensureMapped lm offset readSize
>               (winStart, winData) <- readIORef (lmWindow lm)
>               let relOffset = offset - winStart
>                   chunk = BS.take (fromIntegral readSize) $ BS.drop (fromIntegral relOffset) winData
>                   pieces = BS.split lfByte chunk -- split on LF
>                   chunkEnded = (not (BS.null chunk)) && (BS.last chunk == lfByte)
>               -- build list of complete pieces, and carry a new partial if chunk didn't end with NL
>               let (textsBS, newPartial) = case pieces of
>                     [] -> ([], partial)
>                     [only] -> if chunkEnded
>                                 then ([BS.append partial only], BS.empty)
>                                 else ([], BS.append partial only)
>                     (p:ps) -> if chunkEnded
>                                 then ((BS.append partial p) : ps, BS.empty)
>                                 else ((BS.append partial p) : init ps, last ps)
>                   texts = map decodeUtf8Lenient textsBS
>                   numNew = length texts
>               -- collect requested slice
>               let allLines = texts
>                   needed = remaining
>                   toTake = take needed (drop (fromIntegral (startLine - curLine)) allLines)
>                   acc' = acc ++ map normalizeLine toTake
>               if length toTake >= needed
>                 then return acc'
>                 else do
>                   let newCur = curLine + fromIntegral numNew
>                       newRemaining = remaining - length toTake
>                       newOffset = offset + fromIntegral readSize
>                   go newCur acc' newRemaining newPartial newOffset
>   go baseLine [] count BS.empty off

Text Processing Utilities
--------------------------

Normalize Windows line endings and handle UTF-8 decoding leniently
(replacing invalid sequences rather than crashing).

> normalizeLine :: T.Text -> T.Text
> normalizeLine = T.dropWhileEnd (== '\r')
>
> decodeUtf8Lenient :: BS.ByteString -> T.Text
> decodeUtf8Lenient = TE.decodeUtf8With TEE.lenientDecode

Design Trade-offs and Future Enhancements
------------------------------------------

Current limitations and potential improvements:

1. **Bidirectional Index for Negative Indexing Optimization** (PRIORITY)
   
   Current Issue:
   - Negative indexing (e.g., getLines lm (-5) 5 for last 5 lines) requires
     full file scan to count total lines
   - This defeats the purpose of sparse indexing for end-of-file access
   
   Proposed Solution:
   - Maintain two separate sparse indexes:
     * lmForward: offsets from start (0, K, 2K, ...) [EXISTING]
     * lmBackward: offsets from end (EOF, EOF-K, EOF-2K, ...)
   
   - When negative index requested:
     * Use hFileSize to get file size (O(1) syscall)
     * Scan backward from EOF building backward index
     * Convert negative index to absolute once we know line position
   
   - Keep indexes separate until they overlap:
     * Track coverage: forwardCoverage and backwardCoverage (line ranges)
     * When ranges meet, merge into single unified index
     * After merge, all access becomes O(1) lookup
   
   Implementation Strategy:
   
   a) Data Structure Changes:
      ```
      data LineMap = LineMap
        { ...
        , lmBackward  :: IORef (VS.Vector Word64)  -- NEW: EOF-relative offsets
        , lmBackLines :: IORef Integer              -- NEW: lines counted from end
        , lmMerged    :: IORef Bool                 -- NEW: indexes merged flag
        }
      ```
   
   b) Backward Scanning Algorithm:
      - Start at EOF, read chunks in reverse
      - Count newlines to find K-line boundaries
      - Store absolute byte offsets (not EOF-relative)
      - Challenge: Must handle partial lines at chunk boundaries in reverse
   
   c) Convergence Detection:
      - Forward reaches line M: forwardCoverage = [0, M]
      - Backward reaches line N from end: backwardCoverage = [N, total]
      - Converged when M >= N or when they overlap
      - Merge by sorting all cached offsets
   
   d) Benefits:
      - EOF access becomes O(1) after first backward scan to target
      - "Jump to end" pattern is fast (common in log viewing)
      - Eventually converges to full index without upfront cost
      - Memory cost is same as unidirectional approach
   
   e) Edge Cases:
      - Empty file: both indexes empty, converged immediately
      - Small file (< 2K lines): indexes meet on first access
      - Very large file: indexes may never converge (acceptable)

2. **Index Persistence**: Index rebuilds on each file open
   - Could save/load index to sidecar file for instant reopening
   - Format: simple binary (line count, K, offsets array)
   - Validate with file size/mtime to detect changes
   
3. **Concurrent Safety**: Multiple threads modifying same LineMap unsafe
   - Could wrap IORefs in MVar or use STM for thread-safe access
   - Needed for web server handling concurrent requests
   
4. **Memory-mapped I/O**: Currently uses Handle-based reads
   - Could use mmap for potentially better OS-level caching
   - Particularly effective for repeated access patterns
   
5. **Adaptive Index Density**: Fixed K parameter
   - Could adjust K based on file size or access patterns
   - Smaller K for frequently-accessed regions

The current design prioritizes simplicity and correctness while still
providing good performance for gigabyte-scale files in typical viewer usage.
The bidirectional index enhancement would be the most impactful next step.
