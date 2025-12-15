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
> import System.IO
> import Data.Word
> import Data.IORef
> import Control.Exception
> import Control.Monad (when)

Core Data Types
---------------

The LineMap encapsulates both the file handle and the lazily-built index.
Using IORef allows the index to grow transparently as the file is accessed,
without requiring callers to thread state through every operation.

LineMap is intentionally kept opaque to the user of the module.

> type Offset = Integer
>
> data LineMap = LineMap
>   { lmPath      :: FilePath
>   , lmHandle    :: Handle
>   , lmIndexStep :: Int
>   , lmForward   :: IORef (VS.Vector Word64) -- byte offsets at lines 0, K, 2K, ...
>   , lmIndexed   :: IORef Integer -- how many lines have been scanned from start
>   }

The default index step of 1024 provides good balance:
- Memory: ~8 bytes per 1024 lines = 8KB per million lines
- Speed: Maximum 1024-line scan to reach any target

> indexStepDefault :: Int
> indexStepDefault = 1024

Public API
----------

The API is deliberately minimal to support the two primary use cases:
1. Web server: cache LineMap and serve line ranges to clients
2. CUI app: hold LineMap during navigation session

Opening a file initializes the sparse index with just offset 0 (start of file).
All other index entries build lazily as regions are accessed.

> openLineMap :: FilePath -> Int -> IO LineMap
> openLineMap path k = do
>   h <- openBinaryFile path ReadMode
>   fref <- newIORef (VS.singleton 0) -- offset 0 at line 0
>   iref <- newIORef 0
>   return $ LineMap path h k fref iref
>
> closeLineMap :: LineMap -> IO ()
> closeLineMap lm = hClose (lmHandle lm)

Line Access with Negative Indexing
-----------------------------------

The primary operation returns a slice of lines starting at a given position.
Negative indices count from end-of-file (Python-style).

Design decision: When negative indexing is used, we perform a full scan to
count total lines. This is acceptable because:
1. Negative indexing is rare (mostly "jump to end")
2. The scan result could be cached if needed
3. Alternative would require backward index building (complex)

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

Helper: Count Total Lines
--------------------------

Simple full-file scan counting newlines. Opens a separate handle to avoid
interfering with the main handle's position.

> countTotalLines :: LineMap -> IO Integer
> countTotalLines lm = do
>   h <- openBinaryFile (lmPath lm) ReadMode
>   finally (go h 0) (hClose h)
>   where
>     go h acc = do
>       bs <- BS.hGet h 65536
>       if BS.null bs then return acc
>       else do
>         let n = fromIntegral $ BS.count 10 bs -- count LF
>         go h (acc + n)

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
>         scanAndBuildIndex lm startOffset startLine baseLine

Index Building Strategy
-----------------------

When scanning forward, we read in chunks and count newlines. Whenever we cross
a K-line boundary, we precisely locate that boundary's byte offset and cache it.

This "scan and record crossings" approach ensures:
- Index remains accurate (exact byte offsets)
- No redundant work (we're scanning anyway to reach the target)
- Index density matches access pattern (frequently accessed regions get indexed)

> scanAndBuildIndex :: LineMap -> Offset -> Integer -> Integer -> IO Offset
> scanAndBuildIndex lm startOffset startLine targetLine = do
>   hSeek (lmHandle lm) AbsoluteSeek startOffset
>   let k = fromIntegral (lmIndexStep lm)
>       buf = 65536
>       loop offset curLine = do
>         if curLine >= targetLine
>           then return offset
>           else do
>             bs <- BS.hGet (lmHandle lm) buf
>             if BS.null bs
>               then return offset
>               else do
>                 let newlines = fromIntegral $ BS.count 10 bs
>                     newOffset = offset + fromIntegral (BS.length bs)
>                 -- Check if we crossed any index boundaries and record them
>                 let oldIdx = curLine `div` k
>                     newLine = curLine + newlines
>                     newIdx = newLine `div` k
>                 when (newIdx > oldIdx) $ do
>                   -- We crossed index boundaries, need to find exact offsets
>                   recordIndexCrossings lm offset curLine bs k (oldIdx + 1) newIdx
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
>         | b == 10 = findOff (off + 1) (line + 1) bs
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
>   let h = lmHandle lm
>   hSeek h AbsoluteSeek off
>   let go curLine acc remaining partial = do
>         if remaining <= 0 then return acc
>         else do
>           bs <- BS.hGet h 65536
>           if BS.null bs
>             then do -- EOF: include partial only if it falls within requested window
>               let includePartial = (not (BS.null partial)) && (curLine >= startLine) && (remaining > 0)
>                   acc' = if includePartial then acc ++ [decodeUtf8Lenient partial] else acc
>               return acc'
>             else do
>               let pieces = BS.split 10 bs -- split on LF
>                   chunkEnded = (not (BS.null bs)) && (BS.last bs == 10)
>               -- build list of complete pieces, and carry a new partial if chunk didn't end with NL
>               let (textsBS, newPartial) = case pieces of
>                     [] -> ([], partial)
>                     [only] -> if chunkEnded
>                                 then ([if BS.null partial then only else BS.concat [partial, only]], BS.empty)
>                                 else ([], if BS.null partial then only else BS.concat [partial, only])
>                     (p:ps) -> if chunkEnded
>                                 then ( (if BS.null partial then p else BS.concat [partial,p]) : ps , BS.empty)
>                                 else let mid = init ps
>                                          lastp = last ps
>                                          headCombined = if BS.null partial then p else BS.concat [partial,p]
>                                      in ( headCombined : mid, lastp )
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
>                   go newCur acc' newRemaining newPartial
>   go baseLine [] count BS.empty

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

1. **No backward index**: Negative indexing requires full scan
   - Could add sparse backward index for "jump to end" optimization
   
2. **No index persistence**: Index rebuilds on each file open
   - Could save/load index to sidecar file for instant reopening
   
3. **No concurrent safety**: Multiple threads modifying same LineMap unsafe
   - Could wrap IORefs in MVar or use STM for thread-safe access
   
4. **Memory-mapped I/O**: Currently uses Handle-based reads
   - Could use mmap for potentially better OS-level caching
   
5. **Adaptive index density**: Fixed K parameter
   - Could adjust K based on file size or access patterns

The current design prioritizes simplicity and correctness while still
providing good performance for gigabyte-scale files in typical viewer usage.
