Sparse Line Map v2 - Unified Forward/Backward Index
====================================================

This is an experimental redesign using an inverted index structure where:
- Keys are byte offsets (absolute)
- Values are line numbers (Forward or Backward reference frame)
- Single unified table from the start
- Simpler merging and convergence

Module Header
-------------

> {-# LANGUAGE OverloadedStrings #-}
> module HaFileViewer.LineMap2
>   ( LineMap
>   , openLineMap
>   , closeLineMap
>   , getLines
>   , indexStepDefault
>   ) where
>
> import qualified Data.Map.Strict as Map
> import qualified Data.ByteString as BS
> import qualified Data.Text as T
> import Data.List (unsnoc)
> import System.IO.MMap (mmapFileByteString)
> import System.IO (withFile, IOMode(..), hFileSize)
> import Data.IORef
> import Control.Monad (when)
> import HaFileViewer.LineMap.Common
>   ( Offset
>   , lfByte
>   , normalizeLine
>   , decodeUtf8Lenient
>   , ensureMapped
>   , readAtOffset
>   )

Core Data Types
---------------

Line numbers can be counted from start (Forward) or end (Backward).

> data LineIndex = Forward Integer | Backward Integer
>   deriving (Show, Eq, Ord)

Convert to absolute line number when total is known.

> toAbsoluteLine :: Integer    -- ^ Total number of lines in file
>                -> LineIndex  -- ^ Line index (Forward or Backward)
>                -> Integer    -- ^ Absolute line number (0-based)
> toAbsoluteLine _total (Forward n)  = n
> toAbsoluteLine total  (Backward n) = total - n

Check if we can use this entry (Forward always usable, Backward only if total known).

> isUsable :: Maybe Integer  -- ^ Total lines (if known)
>          -> LineIndex      -- ^ Line index to check
>          -> Bool           -- ^ Whether the index is usable
> isUsable _         (Forward _)  = True
> isUsable (Just _)  (Backward _) = True
> isUsable Nothing   (Backward _) = False

The LineMap with unified index structure.

> type IndexMap = Map.Map Offset LineIndex
>
> data LineMap = LineMap
>   { lmPath        :: FilePath
>   , lmFileSize    :: Integer
>   , lmWindowSize  :: Integer
>   , lmWindow      :: IORef (Offset, BS.ByteString)
>   , lmIndexStep   :: Int
>   , lmIndex       :: IORef IndexMap           -- Unified offset-to-line map
>   , lmTotalLines  :: IORef (Maybe Integer)    -- Cached total
>   , lmBackIndexed :: IORef Integer            -- Lines scanned from EOF
>   , lmBackOffset  :: IORef Offset             -- Furthest offset scanned backward to
>   }                                           -- CRITICAL: lmBackIndexed and lmBackOffset must
>                                               -- always refer to the same K-boundary position!
>                                               -- See computeTotalFromIndexes for why.

Default index step.

> indexStepDefault :: Int
> indexStepDefault = 1024

Default window size for memory mapping.

> windowSizeDefault :: Integer
> windowSizeDefault = 100 * 1024 * 1024

Default chunk size for sequential scanning.

> scanChunkSizeDefault :: Integer
> scanChunkSizeDefault = 65536  -- 64 KB: good balance between cache locality and I/O efficiency

Public API
----------

Open a file and create a LineMap for efficient line-based access.

> openLineMap :: FilePath  -- ^ Path to the file to open
>             -> Int       -- ^ Index step (lines between index points)
>             -> IO LineMap  -- ^ Initialized line map
> openLineMap path k = do
>   fileSize <- withFile path ReadMode (fmap fromIntegral . hFileSize)
>   let winSize = min windowSizeDefault fileSize
>       mapSize = if winSize > 0 then Just (0, fromIntegral winSize) else Nothing
>   initialWindow <- if fileSize > 0
>                    then mmapFileByteString path mapSize
>                    else return BS.empty
>   winRef <- newIORef (0, initialWindow)
>   -- Start with just offset 0 → line 0
>   indexRef <- newIORef (Map.singleton 0 (Forward 0))
>   totalRef <- newIORef Nothing
>   backIndexedRef <- newIORef 0
>   backOffsetRef <- newIORef fileSize  -- Will be updated on first backward scan
>   return $ LineMap path fileSize windowSizeDefault winRef k indexRef totalRef backIndexedRef backOffsetRef

Close a LineMap and release resources.

> closeLineMap :: LineMap  -- ^ The line map to close
>              -> IO ()
> closeLineMap _lm = return ()

Main API function.

Negative Indexing Helpers
--------------------------

Convert negative index to positive using cached total and retrieve lines.

> handleNegativeWithCache :: LineMap   -- ^ The line map
>                         -> Integer   -- ^ Negative start index
>                         -> Int       -- ^ Number of lines to read
>                         -> Integer   -- ^ Cached total line count
>                         -> IO [T.Text]  -- ^ Retrieved lines
> handleNegativeWithCache lm start count total = do
>   let startPos = max 0 (total + start)
>       k = fromIntegral (lmIndexStep lm)
>       baseLine = (startPos `div` k) * k
>   baseOffset <- findOrScanTo lm baseLine
>   scanLinesFromOffset lm baseOffset baseLine startPos count

Handle case where backward scan reached file start (total is now known).

> handleNegativeReachedStart :: LineMap   -- ^ The line map
>                            -> Integer   -- ^ Negative start index
>                            -> Int       -- ^ Number of lines to read
>                            -> Integer   -- ^ Computed total line count
>                            -> IO [T.Text]  -- ^ Retrieved lines
> handleNegativeReachedStart lm start count total = do
>   writeIORef (lmTotalLines lm) (Just total)
>   let startPos = max 0 (total + start)
>       k = fromIntegral (lmIndexStep lm)
>       baseLine = (startPos `div` k) * k
>   baseOffset <- findOrScanTo lm baseLine
>   scanLinesFromOffset lm baseOffset baseLine startPos count

Handle case where total must be computed from partial indexes.

> handleNegativeComputeTotal :: LineMap   -- ^ The line map
>                            -> Integer   -- ^ Negative start index
>                            -> Int       -- ^ Number of lines to read
>                            -> Integer   -- ^ Absolute value of start (lines from end)
>                            -> Integer   -- ^ Lines scanned backward so far
>                            -> IO [T.Text]  -- ^ Retrieved lines
> handleNegativeComputeTotal lm start count linesFromEnd backLinesScanned = do
>   let k = fromIntegral (lmIndexStep lm)
>   if backLinesScanned < k
>     then extractLinesFromBackwardScan lm linesFromEnd count
>     else do
>       total <- computeTotalFromIndexes lm
>       writeIORef (lmTotalLines lm) (Just total)
>       let startPos = max 0 (total + start)
>           baseLine = (startPos `div` k) * k
>       baseOffset <- findOrScanTo lm baseLine
>       scanLinesFromOffset lm baseOffset baseLine startPos count

Positive Indexing Helper
-------------------------

Handle forward/positive indexing - find K-boundary and scan forward.

> handlePositiveIndex :: LineMap   -- ^ The line map
>                     -> Integer   -- ^ Starting line (0-based)
>                     -> Int       -- ^ Number of lines to read
>                     -> IO [T.Text]  -- ^ Retrieved lines
> handlePositiveIndex lm start count = do
>   let k = fromIntegral (lmIndexStep lm)
>       baseLine = (start `div` k) * k
>   baseOffset <- findOrScanTo lm baseLine
>   lns <- scanLinesFromOffset lm baseOffset baseLine start count
>   -- If we got fewer lines than requested, we hit EOF - cache the total
>   when (length lns < count) $ do
>     let totalLines = start + fromIntegral (length lns)
>     writeIORef (lmTotalLines lm) (Just totalLines)
>   return lns

> getLines :: LineMap -> Integer -> Int -> IO [T.Text]
> getLines lm start count
>   | count <= 0 = return []
>   | lmFileSize lm == 0 = return []
>   | start >= 0 = handlePositiveIndex lm start count
>   | otherwise = do
>       -- Negative indexing - check cache first
>       cachedTotal <- readIORef (lmTotalLines lm)
>       case cachedTotal of
>         Just total -> handleNegativeWithCache lm start count total
>         Nothing -> do
>           -- No cached total - use backward index
>           let linesFromEnd = abs start
>               targetLinesFromEnd = linesFromEnd + fromIntegral count
>           -- Scan backward if needed
>           backLinesScanned <- readIORef (lmBackIndexed lm)
>           when (backLinesScanned < targetLinesFromEnd) $ do
>             scanBackwardAndBuildIndex lm targetLinesFromEnd
>             checkAndMergeIndexes lm
>           -- Check results
>           backLinesScanned' <- readIORef (lmBackIndexed lm)
>           idx <- readIORef (lmIndex lm)
>           let hasBackwardAtStart = Map.member 0 idx && case Map.lookup 0 idx of
>                                      Just (Backward _) -> True
>                                      _ -> False
>           
>           if hasBackwardAtStart
>             then do
>               -- Reached start of file, total is known
>               let Just (Backward total) = Map.lookup 0 idx
>               handleNegativeReachedStart lm start count total
>             else handleNegativeComputeTotal lm start count linesFromEnd backLinesScanned'

Helper: Windowed Memory Mapping
-------------------------------

Wrapper functions that use the common utilities with LineMap fields.

> ensureMappedLM :: LineMap   -- ^ The line map
>                -> Offset    -- ^ Offset to ensure is mapped
>                -> Integer   -- ^ Number of bytes to ensure are mapped
>                -> IO ()
> ensureMappedLM lm = ensureMapped (lmPath lm) (lmFileSize lm) (lmWindowSize lm) (lmWindow lm)

Read bytes at a specific offset, handling window remapping if needed.

> readAtOffsetLM :: LineMap    -- ^ The line map
>                -> Offset     -- ^ Starting offset to read from
>                -> Integer    -- ^ Number of bytes to read
>                -> IO BS.ByteString  -- ^ The bytes read
> readAtOffsetLM lm = readAtOffset (lmPath lm) (lmFileSize lm) (lmWindowSize lm) (lmWindow lm)

Core Index Operations
---------------------

Find or scan to a target line, returning the byte offset.

> findOrScanTo :: LineMap   -- ^ The line map
>              -> Integer   -- ^ Target line number (0-based)
>              -> IO Offset  -- ^ Byte offset at start of target line
> findOrScanTo lm targetLine
>   | targetLine == 0 = return 0
>   | otherwise = do
>       idx <- readIORef (lmIndex lm)
>       cachedTotal <- readIORef (lmTotalLines lm)
>       

>       -- Find closest indexed line at or before target
>       let usableEntries = [(off, ln) | (off, li) <- Map.toList idx
>                                       , isUsable cachedTotal li
>                                       , let ln = case li of
>                                               Forward n -> n
>                                               Backward n -> case cachedTotal of
>                                                 Just t -> t - n
>                                                 Nothing -> -1
>                                       , ln >= 0 && ln <= targetLine]
>       

>       case usableEntries of
>         [] -> do
>           -- No usable index, scan from beginning
>           scanToLine lm 0 0 targetLine
>         _ -> do
>           let (closestLine, closestOff) = maximum [(ln, off) | (off, ln) <- usableEntries]

>           if closestLine == targetLine
>             then return closestOff
>             else scanToLine lm closestOff closestLine targetLine

Scan from a known offset/line to find the target line.

Helper: Record Forward index entries at K-boundaries for newlines in a chunk.

> recordForwardIndexEntries :: LineMap   -- ^ The line map to update
>                           -> Integer   -- ^ Index step K (record every K lines)
>                           -> [Int]     -- ^ Byte positions of newlines in chunk
>                           -> Integer   -- ^ Line number at start of chunk (0-based)
>                           -> Offset    -- ^ Byte offset at start of chunk
>                           -> IO ()
> recordForwardIndexEntries lm k positions lineNum offset = do
>   let recordIndex ln off =
>         when (ln `mod` k == 0) $ do
>           modifyIORef' (lmIndex lm) (Map.insert off (Forward ln))
>   mapM_ (\i -> let pos = positions !! i
>                    ln = lineNum + fromIntegral i + 1
>                    off = offset + fromIntegral pos + 1
>                in recordIndex ln off)
>         [0 .. length positions - 1]

Scan from a known position to find a target line.

> scanToLine :: LineMap    -- ^ The line map for file access and indexing
>            -> Offset     -- ^ Starting byte offset (known position)
>            -> Integer    -- ^ Line number at start offset (0-based)
>            -> Integer    -- ^ Target line number to find (0-based)
>            -> IO Offset  -- ^ Byte offset at start of target line
> scanToLine lm startOffset startLine targetLine = do
>   let k = fromIntegral (lmIndexStep lm)
>       linesToScan = targetLine - startLine
>       chunkSize = scanChunkSizeDefault
>       
>       loop offset lineNum linesLeft = do
>         if linesLeft <= 0
>           then return offset
>           else do
>             let readSize = min chunkSize (lmFileSize lm - offset)
>             if readSize <= 0
>               then return offset
>               else do
>                 chunk <- readAtOffsetLM lm offset readSize
>                 let positions = BS.elemIndices lfByte chunk
>                     linesInChunk = length positions
>                 
>                 if linesInChunk >= fromIntegral linesLeft
>                   then do
>                     -- Target is in this chunk
>                     let targetPos = positions !! (fromIntegral linesLeft - 1)
>                         finalOffset = offset + fromIntegral targetPos + 1
>                     -- Record index entries at K boundaries
>                     recordForwardIndexEntries lm k positions lineNum offset
>                     return finalOffset
>                   else do
>                     -- Keep scanning
>                     recordForwardIndexEntries lm k positions lineNum offset
>                     let newOffset = offset + fromIntegral (BS.length chunk)
>                         newLineNum = lineNum + fromIntegral linesInChunk
>                         newLinesLeft = linesLeft - fromIntegral linesInChunk
>                     loop newOffset newLineNum newLinesLeft
>   
>   loop startOffset startLine linesToScan

Line Iterator - Explicit State Passing
---------------------------------------

Stateful iterator for reading lines sequentially with explicit state.
This provides a composable way to read lines without managing complex
chunking logic. State is immutable and threaded explicitly.

> data LineIterator = LineIterator
>   { liOffset  :: Offset            -- Current byte position in file
>   , liPartial :: BS.ByteString     -- Incomplete line from previous chunk
>   , liBuffer  :: [BS.ByteString]   -- Buffered lines from last chunk read (raw bytes)
>   } deriving (Show)

Create a new iterator starting at a given offset.

> createLineIterator :: Offset         -- ^ Starting byte offset in file
>                    -> LineIterator   -- ^ New iterator at that position
> createLineIterator offset = LineIterator offset BS.empty []

Get the next line from the iterator, returning updated state (raw ByteString).

> nextLine :: LineMap       -- ^ The line map for file access
>          -> LineIterator  -- ^ Current iterator state
>          -> IO (Maybe BS.ByteString, LineIterator)  -- ^ Next line (raw) and new state
> nextLine lm iter@(LineIterator offset partial buffer) = do
>   case buffer of
>     (line:rest) -> 
>       -- Return buffered line
>       return (Just line, iter { liBuffer = rest })
>     [] -> do
>       -- Buffer empty, read next chunk
>       let fileSize = lmFileSize lm
>       if offset >= fileSize
>         then 
>           -- EOF - return final partial line if exists
>           if BS.null partial
>             then return (Nothing, iter)
>             else return (Just partial,
>                         iter { liPartial = BS.empty })
>         else do
>           -- Read and process next chunk
>           let readSize = min scanChunkSizeDefault (fileSize - offset)
>           chunk <- readAtOffsetLM lm offset readSize
>           let pieces = BS.split lfByte chunk
>               chunkEnded = (not (BS.null chunk)) && (BS.last chunk == lfByte)
>               (rawLines, newPartial) = case pieces of
>                 [] -> ([], partial)
>                 [only] -> if chunkEnded
>                             then ([BS.append partial only], BS.empty)
>                             else ([], BS.append partial only)
>                 (p:ps) -> if chunkEnded
>                             then ((BS.append partial p) : ps, BS.empty)
>                             else case unsnoc ps of
>                               Nothing -> ([BS.append partial p], BS.empty)
>                               Just (initPs, lastPs) ->
>                                 ((BS.append partial p) : initPs, lastPs)
>               newIter = LineIterator (offset + readSize) newPartial rawLines
>           nextLine lm newIter  -- Recursively get first line from new buffer

Skip N lines efficiently using the iterator.

> skipLines :: LineMap       -- ^ The line map for file access
>           -> LineIterator  -- ^ Current iterator state
>           -> Int           -- ^ Number of lines to skip
>           -> IO LineIterator  -- ^ Updated iterator after skipping
> skipLines _lm iter 0 = return iter
> skipLines lm iter n = do
>   (mbLine, iter') <- nextLine lm iter
>   case mbLine of
>     Nothing -> return iter'  -- Hit EOF
>     Just _  -> skipLines lm iter' (n - 1)

Collect N lines using the iterator, decoding to Text.

> collectLines :: LineMap       -- ^ The line map for file access
>              -> LineIterator  -- ^ Current iterator state
>              -> Int           -- ^ Number of lines to collect
>              -> IO [T.Text]   -- ^ Collected lines (decoded and normalized)
> collectLines lm iter count = go iter count []
>   where
>     go _iter 0 acc = return (reverse acc)
>     go curIter n acc = do
>       (mbLine, newIter) <- nextLine lm curIter
>       case mbLine of
>         Nothing -> return (reverse acc)
>         Just rawLine -> 
>           let decodedLine = normalizeLine (decodeUtf8Lenient rawLine)
>           in go newIter (n - 1) (decodedLine : acc)

Scan lines from a given offset and return them as Text.

This function reads a range of lines from a file, starting from a known
byte offset and line number. It handles:
- Lines split across chunk boundaries
- Files that don't end with a newline
- Skipping lines before the target
- Windows (CRLF) and Unix (LF) line endings

> scanLinesFromOffset :: LineMap   -- ^ The line map with file access
>                     -> Offset     -- ^ Starting byte offset in file
>                     -> Integer    -- ^ Line number at startOffset (0-based)
>                     -> Integer    -- ^ Target line number to start reading (0-based)
>                     -> Int        -- ^ Number of lines to read
>                     -> IO [T.Text]  -- ^ List of text lines (without newlines)
> scanLinesFromOffset lm startOffset startLine targetLine count = do
>   let iter0 = createLineIterator startOffset
>       skipCount = fromIntegral (targetLine - startLine)
>   iter1 <- skipLines lm iter0 skipCount
>   collectLines lm iter1 count

Backward Scanning
-----------------

Scan backward from EOF to build Backward index entries.

> scanBackwardAndBuildIndex :: LineMap   -- ^ The line map
>                           -> Integer   -- ^ Target lines from end to scan
>                           -> IO ()
> scanBackwardAndBuildIndex lm targetLinesFromEnd = do
>   backLinesScanned <- readIORef (lmBackIndexed lm)
>   when (backLinesScanned < targetLinesFromEnd) $ do
>     let k = fromIntegral (lmIndexStep lm)
>         fileSize = lmFileSize lm
>         chunkSize = scanChunkSizeDefault
>     
>     -- Initialize based on whether we've scanned before and whether file ends with LF
>     initLinesFromEnd <- if backLinesScanned > 0
>       then return backLinesScanned
>       else if fileSize == 0
>         then return 0
>         else do
>           lastByteChunk <- readAtOffsetLM lm (fileSize - 1) 1
>           let lastByte = BS.head lastByteChunk
>               endsWithNL = lastByte == lfByte
>           return $ if endsWithNL then 0 else 1
>     
>     let loop offset linesFromEnd = do
>           if linesFromEnd >= targetLinesFromEnd + k || offset <= 0
>             then return ()
>             else do
>               let readSize = min chunkSize offset
>                   readStart = offset - readSize
>               chunk <- readAtOffsetLM lm readStart readSize
>               let lfPositions = BS.elemIndices lfByte chunk
>                   linesInChunk = length lfPositions
>                   
>                   recordIndex i = do
>                     let pos = lfPositions !! i
>                         absoluteOffset = readStart + fromIntegral pos + 1
>                         linesFromEndHere = linesFromEnd + fromIntegral (linesInChunk - i - 1)
>                     when (linesFromEndHere `mod` k == 0) $ do
>                       modifyIORef' (lmIndex lm) (Map.insert absoluteOffset (Backward linesFromEndHere))
>                       -- CRITICAL: Update tracked values to match this K-boundary entry.
>                       -- lmBackIndexed and lmBackOffset must ALWAYS refer to an actual indexed
>                       -- K-boundary position, not arbitrary chunk boundaries. This ensures that
>                       -- computeTotalFromIndexes correctly calculates: total = linesFromStart + backIndexed
>                       -- where linesFromStart is newlines from 0 to lmBackOffset, and backIndexed is
>                       -- lines from lmBackOffset to EOF, both referring to the same offset.
>                       writeIORef (lmBackIndexed lm) linesFromEndHere
>                       writeIORef (lmBackOffset lm) absoluteOffset
>               
>               mapM_ recordIndex [linesInChunk - 1, linesInChunk - 2 .. 0]
>               
>               let newLinesFromEnd = linesFromEnd + fromIntegral linesInChunk
>               -- Note: lmBackIndexed and lmBackOffset are updated in recordIndex for K-boundaries
>               -- We don't update them here to keep them at the last K-boundary
>               if readStart == 0
>                 then do
>                   -- Reached beginning
>                   modifyIORef' (lmIndex lm) (Map.insert 0 (Backward newLinesFromEnd))
>                   -- Update to reflect reaching the start
>                   writeIORef (lmBackIndexed lm) newLinesFromEnd
>                   writeIORef (lmBackOffset lm) 0
>                   return ()
>                 else loop readStart newLinesFromEnd
>     
>     loop fileSize initLinesFromEnd

Count how many backward lines have been scanned.

> countBackwardLines :: LineMap      -- ^ The line map
>                    -> IO Integer   -- ^ Number of lines scanned from end
> countBackwardLines lm = do
>   idx <- readIORef (lmIndex lm)
>   let backwardEntries = [n | Backward n <- Map.elems idx]
>   return $ if null backwardEntries then 0 else maximum backwardEntries

Extract lines directly from backward scan without computing total.

> extractLinesFromBackwardScan :: LineMap      -- ^ The line map
>                              -> Integer      -- ^ Lines from end to start
>                              -> Int          -- ^ Number of lines to extract
>                              -> IO [T.Text]  -- ^ Extracted lines
> extractLinesFromBackwardScan lm linesFromEnd count = do
>   let fileSize = lmFileSize lm
>       chunkSize = scanChunkSizeDefault
>       targetLines = fromIntegral linesFromEnd + fromIntegral count - 1
>       
>       loop offset accLines = do
>         if offset <= 0 || length accLines >= targetLines
>           then return accLines
>           else do
>             let chunkStart = max 0 (offset - chunkSize)
>                 readSize = offset - chunkStart
>             chunk <- readAtOffsetLM lm chunkStart readSize
>             
>             let pieces = BS.split lfByte chunk
>                 chunkEndsWithLF = not (BS.null chunk) && BS.last chunk == lfByte
>                 validPieces = if chunkEndsWithLF && not (null pieces)
>                               then init pieces
>                               else pieces
>                 
>                 newLines = map (normalizeLine . decodeUtf8Lenient) validPieces
>                 newAccLines = newLines ++ accLines
>             
>             loop chunkStart newAccLines
>   
>   allLines <- loop fileSize []
>   let result = drop (length allLines - count) allLines
>   return result
>
> extractSegments :: BS.ByteString -> [Int] -> [T.Text]
> extractSegments chunk positions =
>   let indices = zip (0 : map (+1) positions) (positions ++ [BS.length chunk])
>   in [decodeUtf8Lenient (BS.take (end - start) (BS.drop start chunk))
>       | (start, end) <- indices, start < end]

Convergence Detection
---------------------

Check if forward and backward indexes have converged.

> checkAndMergeIndexes :: LineMap  -- ^ The line map
>                      -> IO ()
> checkAndMergeIndexes lm = do
>   cachedTotal <- readIORef (lmTotalLines lm)
>   case cachedTotal of
>     Just _ -> return ()  -- Already have total
>     Nothing -> do
>       idx <- readIORef (lmIndex lm)
>       let forwardOffsets = [off | (off, Forward _) <- Map.toList idx]
>           backwardOffsets = [off | (off, Backward _) <- Map.toList idx]
>       
>       when (not (null forwardOffsets) && not (null backwardOffsets)) $ do
>         let maxFwd = maximum forwardOffsets
>             minBack = minimum backwardOffsets
>         when (maxFwd >= minBack) $ do
>           -- Converged - compute total
>           total <- computeTotalFromIndexes lm
>           writeIORef (lmTotalLines lm) (Just total)

Compute total lines from converged indexes.

> computeTotalFromIndexes :: LineMap -> IO Integer
> computeTotalFromIndexes lm = do
>   backIndexed <- readIORef (lmBackIndexed lm)
>   lastBackOffset <- readIORef (lmBackOffset lm)
>   
>   if backIndexed == 0
>     then do
>       -- No backward index, scan entire file
>       countTotalLines lm
>     else do
>       -- Count lines from start to the actual furthest scanned offset
>       linesFromStart <- countLinesUpTo lm lastBackOffset

>       -- Total = lines before scan point + lines scanned from EOF
>       -- (no +1 needed because backIndexed already includes initial partial line if any)
>       return (linesFromStart + backIndexed)

Count lines up to a given offset.

Helper: Fold over file chunks with accumulating state.

This is a general streaming fold that processes a file in chunks without
materializing an intermediate list. It's used by multiple counting functions.

> foldFileChunks :: LineMap    -- ^ The line map with file access
>                -> Offset      -- ^ Starting byte offset
>                -> Offset      -- ^ Ending byte offset (exclusive)
>                -> s           -- ^ Initial state
>                -> (s -> BS.ByteString -> IO s)  -- ^ State update function
>                -> IO s        -- ^ Final state
> foldFileChunks lm startOffset endOffset initialState processChunk =
>   loop startOffset initialState
>   where
>     loop offset state
>       | offset >= endOffset = return state
>       | otherwise = do
>           let remaining = endOffset - offset
>               readSize = min scanChunkSizeDefault remaining
>           chunk <- readAtOffsetLM lm offset readSize
>           newState <- processChunk state chunk
>           loop (offset + readSize) newState

Count lines up to a given offset.

> countLinesUpTo :: LineMap      -- ^ The line map
>                -> Offset       -- ^ Target offset to count up to
>                -> IO Integer   -- ^ Number of newlines up to offset
> countLinesUpTo lm targetOffset =
>   foldFileChunks lm 0 targetOffset 0 $ \nlCount chunk ->
>     return $ nlCount + fromIntegral (BS.count lfByte chunk)

Count total lines in file.

> countTotalLines :: LineMap      -- ^ The line map
>                 -> IO Integer   -- ^ Total number of lines
> countTotalLines lm = do
>   let fileSize = lmFileSize lm
>   if fileSize == 0
>     then return 0
>     else do
>       (nlCount, endsWithNL) <- foldFileChunks lm 0 fileSize (0, False) processChunk
>       let lineCount = if endsWithNL then nlCount else nlCount + 1
>       return lineCount
>   where
>     processChunk (count, _) chunk =
>       let newCount = count + fromIntegral (BS.count lfByte chunk)
>           chunkEndsWithNL = not (BS.null chunk) && BS.last chunk == lfByte
>       in return (newCount, chunkEndsWithNL)

