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
> import qualified Data.Text.Encoding as TE
> import qualified Data.Text.Encoding.Error as TEE
> import System.IO.MMap (mmapFileByteString)
> import System.IO (withFile, IOMode(..), hFileSize)
> import Data.Word
> import Data.IORef
> import Control.Monad (when)

Constants
---------

> lfByte :: Word8
> lfByte = 10

Core Data Types
---------------

Line numbers can be counted from start (Forward) or end (Backward).

> data LineIndex = Forward Integer | Backward Integer
>   deriving (Show, Eq, Ord)

Convert to absolute line number when total is known.

> toAbsoluteLine :: Integer -> LineIndex -> Integer
> toAbsoluteLine _total (Forward n)  = n
> toAbsoluteLine total  (Backward n) = total - n

Check if we can use this entry (Forward always usable, Backward only if total known).

> isUsable :: Maybe Integer -> LineIndex -> Bool
> isUsable _         (Forward _)  = True
> isUsable (Just _)  (Backward _) = True
> isUsable Nothing   (Backward _) = False

The LineMap with unified index structure.

> type Offset = Integer
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

Default window size.

> windowSizeDefault :: Integer
> windowSizeDefault = 100 * 1024 * 1024

Public API
----------

> openLineMap :: FilePath -> Int -> IO LineMap
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

> closeLineMap :: LineMap -> IO ()
> closeLineMap _lm = return ()

Main API function.

> getLines :: LineMap -> Integer -> Int -> IO [T.Text]
> getLines lm start count
>   | count <= 0 = return []
>   | lmFileSize lm == 0 = return []
>   | start >= 0 = do
>       -- Positive indexing - use forward scan
>       let k = fromIntegral (lmIndexStep lm)
>           baseLine = (start `div` k) * k
>       baseOffset <- findOrScanTo lm baseLine
>       lns <- scanLinesFromOffset lm baseOffset baseLine start count
>       -- If we got fewer lines than requested, we hit EOF - cache the total
>       when (length lns < count) $ do
>         let totalLines = start + fromIntegral (length lns)
>         writeIORef (lmTotalLines lm) (Just totalLines)
>       return lns
>   | otherwise = do
>       -- Negative indexing - check cache first
>       cachedTotal <- readIORef (lmTotalLines lm)
>       case cachedTotal of
>         Just total -> do
>           -- Use cached total to convert to positive index
>           let startPos = max 0 (total + start)
>               k = fromIntegral (lmIndexStep lm)
>               baseLine = (startPos `div` k) * k
>           baseOffset <- findOrScanTo lm baseLine
>           lns <- scanLinesFromOffset lm baseOffset baseLine startPos count
>           return lns
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
>               writeIORef (lmTotalLines lm) (Just total)
>               let startPos = max 0 (total + start)
>                   k = fromIntegral (lmIndexStep lm)
>                   baseLine = (startPos `div` k) * k
>               baseOffset <- findOrScanTo lm baseLine
>               lns <- scanLinesFromOffset lm baseOffset baseLine startPos count
>               return lns
>             else do
>               -- Didn't reach beginning - check fast path
>               let k = fromIntegral (lmIndexStep lm)
>               if backLinesScanned' < k
>                 then extractLinesFromBackwardScan lm linesFromEnd count
>                 else do
>                   -- Need to compute total
>                   total <- computeTotalFromIndexes lm
>                   writeIORef (lmTotalLines lm) (Just total)
>                   let startPos = max 0 (total + start)
>                       baseLine = (startPos `div` k) * k

>                   baseOffset <- findOrScanTo lm baseLine
>                   lns <- scanLinesFromOffset lm baseOffset baseLine startPos count
>                   return lns

Helper: Windowed Memory Mapping
-------------------------------

> ensureMapped :: LineMap -> Offset -> Integer -> IO ()
> ensureMapped lm offset size = do
>   (winStart, winData) <- readIORef (lmWindow lm)
>   let winEnd = winStart + fromIntegral (BS.length winData)
>       needsRemap = offset < winStart || offset + size > winEnd
>   
>   when needsRemap $ do
>     let halfWin = lmWindowSize lm `div` 2
>         newStart = max 0 (offset - halfWin)
>         maxSize = lmFileSize lm - newStart
>         newSize = min (lmWindowSize lm) maxSize
>         mapSpec = if newSize > 0 
>                   then Just (fromIntegral newStart, fromIntegral newSize)
>                   else Nothing
>     newData <- if newSize > 0
>                then mmapFileByteString (lmPath lm) mapSpec
>                else return BS.empty
>     writeIORef (lmWindow lm) (newStart, newData)

> readAtOffset :: LineMap -> Offset -> Integer -> IO BS.ByteString
> readAtOffset lm offset size = do
>   ensureMapped lm offset size
>   (winStart, winData) <- readIORef (lmWindow lm)
>   let relOffset = offset - winStart
>       chunk = BS.take (fromIntegral size) $ BS.drop (fromIntegral relOffset) winData
>   return chunk

Text Processing
---------------

> normalizeLine :: T.Text -> T.Text
> normalizeLine = T.dropWhileEnd (== '\r')
>
> decodeUtf8Lenient :: BS.ByteString -> T.Text
> decodeUtf8Lenient = TE.decodeUtf8With TEE.lenientDecode

Core Index Operations
---------------------

Find or scan to a target line, returning the byte offset.

> findOrScanTo :: LineMap -> Integer -> IO Offset
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

> scanToLine :: LineMap -> Offset -> Integer -> Integer -> IO Offset
> scanToLine lm startOffset startLine targetLine = do
>   let k = fromIntegral (lmIndexStep lm)
>       linesToScan = targetLine - startLine
>       chunkSize = 65536
>       
>       loop offset lineNum linesLeft = do
>         if linesLeft <= 0
>           then return offset
>           else do
>             let readSize = min chunkSize (lmFileSize lm - offset)
>             if readSize <= 0
>               then return offset
>               else do
>                 chunk <- readAtOffset lm offset readSize
>                 let positions = BS.elemIndices lfByte chunk
>                     linesInChunk = length positions
>                 
>                 if linesInChunk >= fromIntegral linesLeft
>                   then do
>                     -- Target is in this chunk
>                     let targetPos = positions !! (fromIntegral linesLeft - 1)
>                         finalOffset = offset + fromIntegral targetPos + 1

>                     -- Record index entries at K boundaries
>                     let recordIndex ln off =
>                           when (ln `mod` k == 0) $ do
>                             modifyIORef' (lmIndex lm) (Map.insert off (Forward ln))
>                     mapM_ (\i -> let pos = positions !! i
>                                      ln = lineNum + fromIntegral i + 1
>                                      off = offset + fromIntegral pos + 1
>                                  in recordIndex ln off)
>                           [0 .. linesInChunk - 1]
>                     return finalOffset
>                   else do
>                     -- Keep scanning
>                     let newOffset = offset + fromIntegral (BS.length chunk)
>                         newLineNum = lineNum + fromIntegral linesInChunk
>                         newLinesLeft = linesLeft - fromIntegral linesInChunk
>                     -- Record index entries
>                     let recordIndex ln off =
>                           when (ln `mod` k == 0) $ do
>                             modifyIORef' (lmIndex lm) (Map.insert off (Forward ln))
>                     mapM_ (\i -> let pos = positions !! i
>                                      ln = lineNum + fromIntegral i + 1
>                                      off = offset + fromIntegral pos + 1
>                                  in recordIndex ln off)
>                           [0 .. linesInChunk - 1]
>                     loop newOffset newLineNum newLinesLeft
>   
>   loop startOffset startLine linesToScan

Scan lines from a given offset.

> scanLinesFromOffset :: LineMap -> Offset -> Integer -> Integer -> Int -> IO [T.Text]
> scanLinesFromOffset lm startOffset startLine targetLine count = do
>   let fileSize = lmFileSize lm
>       chunkSize = 65536
>       go curLine acc remaining partial offset = do
>         if remaining <= 0 then return acc
>         else do
>           let bytesRemaining = fileSize - offset
>           if bytesRemaining <= 0
>             then do
>               let includePartial = (not (BS.null partial)) && (curLine >= targetLine) && (remaining > 0)
>                   acc' = if includePartial then acc ++ [decodeUtf8Lenient partial] else acc
>               return acc'
>             else do
>               let readSize = min chunkSize bytesRemaining
>               chunk <- readAtOffset lm offset readSize
>               let pieces = BS.split lfByte chunk
>                   chunkEnded = (not (BS.null chunk)) && (BS.last chunk == lfByte)
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
>               let allLines = texts
>                   needed = remaining
>                   toTake = take needed (drop (fromIntegral (targetLine - curLine)) allLines)
>                   acc' = acc ++ map normalizeLine toTake
>               if length toTake >= needed
>                 then return acc'
>                 else do
>                   let newCur = curLine + fromIntegral numNew
>                       newRemaining = remaining - length toTake
>                       newOffset = offset + fromIntegral readSize
>                   go newCur acc' newRemaining newPartial newOffset
>   go startLine [] count BS.empty startOffset

Backward Scanning
-----------------

Scan backward from EOF to build Backward index entries.

> scanBackwardAndBuildIndex :: LineMap -> Integer -> IO ()
> scanBackwardAndBuildIndex lm targetLinesFromEnd = do
>   backLinesScanned <- readIORef (lmBackIndexed lm)
>   when (backLinesScanned < targetLinesFromEnd) $ do
>     let k = fromIntegral (lmIndexStep lm)
>         fileSize = lmFileSize lm
>         chunkSize = 65536
>     
>     -- Initialize based on whether we've scanned before and whether file ends with LF
>     initLinesFromEnd <- if backLinesScanned > 0
>       then return backLinesScanned
>       else if fileSize == 0
>         then return 0
>         else do
>           lastByteChunk <- readAtOffset lm (fileSize - 1) 1
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
>               chunk <- readAtOffset lm readStart readSize
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

> countBackwardLines :: LineMap -> IO Integer
> countBackwardLines lm = do
>   idx <- readIORef (lmIndex lm)
>   let backwardEntries = [n | Backward n <- Map.elems idx]
>   return $ if null backwardEntries then 0 else maximum backwardEntries

Extract lines directly from backward scan without computing total.

> extractLinesFromBackwardScan :: LineMap -> Integer -> Int -> IO [T.Text]
> extractLinesFromBackwardScan lm linesFromEnd count = do
>   let fileSize = lmFileSize lm
>       chunkSize = 65536
>       targetLines = fromIntegral linesFromEnd + fromIntegral count - 1
>       
>       loop offset accLines = do
>         if offset <= 0 || length accLines >= targetLines
>           then return accLines
>           else do
>             let chunkStart = max 0 (offset - chunkSize)
>                 readSize = offset - chunkStart
>             chunk <- readAtOffset lm chunkStart readSize
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

> checkAndMergeIndexes :: LineMap -> IO ()
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

> countLinesUpTo :: LineMap -> Offset -> IO Integer
> countLinesUpTo lm targetOffset = do
>   let chunkSize = 65536
>       loop offset nlCount = do
>         if offset >= targetOffset
>           then return nlCount
>           else do
>             let remaining = targetOffset - offset
>                 readSize = min chunkSize remaining
>             chunk <- readAtOffset lm offset readSize
>             let count = fromIntegral $ BS.count lfByte chunk
>             loop (offset + readSize) (nlCount + count)
>   loop 0 0

Count total lines in file.

> countTotalLines :: LineMap -> IO Integer
> countTotalLines lm = do
>   let fileSize = lmFileSize lm
>   if fileSize == 0
>     then return 0
>     else do
>       let chunkSize = 65536
>           loop offset nlCount = do
>             if offset >= fileSize
>               then return nlCount
>               else do
>                 let remaining = fileSize - offset
>                     readSize = min chunkSize remaining
>                 chunk <- readAtOffset lm offset readSize
>                 let count = fromIntegral $ BS.count lfByte chunk
>                 loop (offset + readSize) (nlCount + count)
>       nlCount <- loop 0 0
>       -- Check if file ends with newline
>       lastByteChunk <- readAtOffset lm (fileSize - 1) 1
>       let lastByte = BS.head lastByteChunk
>           endsWithNL = lastByte == lfByte
>           lineCount = if endsWithNL then nlCount else nlCount + 1
>       return lineCount
