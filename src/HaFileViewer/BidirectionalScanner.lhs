Bidirectional Scanner - Symmetric Forward/Backward Line Scanning
================================================================

Experimental module exploring the symmetry between forward and backward scanning.
The key insight: backward scanning from EOF is the inverse of forward scanning from 0,
except for edge cases (EOL at beginning/end of file).

Module Header
-------------

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RankNTypes #-}
> module HaFileViewer.BidirectionalScanner
>   ( Direction(..)
>   , scanLines
>   , ChunkSize
>   , defaultChunkSize
>   ) where
>
> import qualified Data.ByteString as BS
> import qualified Data.Text as T
> import Data.Word (Word8)
> import HaFileViewer.LineMap.Common
>   ( Offset
>   , lfByte
>   , normalizeLine
>   , decodeUtf8Lenient
>   )
> import Debug.Trace (trace)

Core Types
----------

> data Direction = Forward | Backward
>   deriving (Show, Eq)
>
> type ChunkSize = Integer
>
> defaultChunkSize :: ChunkSize
> defaultChunkSize = 65536  -- 64 KB

Partial side indicates which edge of the chunk carries the partial line.

> data PartialSide = LeftPartial | RightPartial
>   deriving (Show, Eq)

Strategy Pattern
----------------

Bundle of functions that vary by direction. This makes the symmetry explicit.

Common extraction logic that both strategies use:

> extractMiddlePieces :: [a] -> [a]
> extractMiddlePieces ps = if length ps < 2 then [] else tail (init ps)

> data ScanStrategy = ScanStrategy
>   { stratHasMore           :: ScanState -> Bool
>   , stratCalcRead          :: ChunkSize -> ScanState -> (Offset, Integer)
>   , stratUpdateOffset      :: Integer -> Offset -> Offset
>   , stratCombineLines      :: [BS.ByteString] -> [BS.ByteString] -> [BS.ByteString]
>   , stratPartialSide       :: PartialSide
>   , stratFinalOrder        :: forall a. [a] -> [a]
>   , stratCanonicalizeChunk :: BS.ByteString -> ScanState -> BS.ByteString
>   , stratOrderPieces       :: [BS.ByteString] -> [BS.ByteString]
>   , stratCombinePartial    :: BS.ByteString -> BS.ByteString -> BS.ByteString
>   , stratGetEdgePiece      :: [BS.ByteString] -> BS.ByteString
>   , stratGetMiddle         :: [BS.ByteString] -> [BS.ByteString]
>   , stratGetNewPartial     :: [BS.ByteString] -> BS.ByteString
>   }

Create strategy for forward scanning.

> forwardStrategy :: ScanStrategy
> forwardStrategy = ScanStrategy
>   { stratHasMore = \s -> ssOffset s < ssFileSize s
>   , stratCalcRead = \chunkSize s ->
>       let offset = ssOffset s
>           remaining = ssFileSize s - offset
>           size = min chunkSize remaining
>       in (offset, size)
>   , stratUpdateOffset = \delta offset -> offset + delta
>   , stratCombineLines = (++)  -- Append to end
>   , stratPartialSide = LeftPartial
>   , stratFinalOrder = id
>   , stratCanonicalizeChunk = \chunk state ->
>       let isLastChunk = ssOffset state + fromIntegral (BS.length chunk) >= ssFileSize state
>           needsLF = isLastChunk && not (ssEndsWithLF state) && not (BS.null chunk)
>       in if needsLF then BS.snoc chunk lfByte else chunk
>   , stratOrderPieces = id  -- Keep pieces in file order
>   , stratCombinePartial = \partial piece -> BS.append partial piece  -- partial on left
>   , stratGetEdgePiece = head                    -- first piece combines with partial
>   , stratGetMiddle = extractMiddlePieces       -- No transformation needed
>   , stratGetNewPartial = \ps -> if null ps then BS.empty else last ps  -- last piece is new partial
>   }

Create strategy for backward scanning - mirrors forward by reversing pieces.

> backwardStrategy :: ScanStrategy
> backwardStrategy = ScanStrategy
>   { stratHasMore = \s -> ssOffset s > 0
>   , stratCalcRead = \chunkSize s ->
>       let offset = ssOffset s
>           size = min chunkSize offset
>           startOffset = offset - size
>       in (startOffset, size)
>   , stratUpdateOffset = \delta offset -> offset - delta
>   , stratCombineLines = flip (++)  -- Prepend to beginning
>   , stratPartialSide = RightPartial  -- Reading backward from EOF, partial on right
>   , stratFinalOrder = id  -- After reversal + RightPartial, already in file order!
>   , stratCanonicalizeChunk = \chunk state ->
>       let isEOFChunk = ssOffset state >= ssFileSize state
>           needsLF = isEOFChunk && not (ssEndsWithLF state) && not (BS.null chunk)
>       in if needsLF then BS.snoc chunk lfByte else chunk
>   , stratOrderPieces = \pieces ->
>       let reversed = reverse pieces
>           dropEmpty ps = if null ps || not (BS.null (head ps))
>                          then ps
>                          else tail ps
>       in dropEmpty reversed  -- Reverse and drop trailing empty
>   , stratCombinePartial = \partial piece -> BS.append partial piece  -- Same as forward!
>   , stratGetEdgePiece = \ps -> if null ps then BS.empty else head ps  -- Same as forward!
>   , stratGetMiddle = reverse . extractMiddlePieces  -- Reverse middle to restore file order!
>   , stratGetNewPartial = \ps -> if null ps then BS.empty else last ps  -- Same as forward!
>   }

Get strategy for a given direction.

> getStrategy :: Direction -> ScanStrategy
> getStrategy Forward  = forwardStrategy
> getStrategy Backward = backwardStrategy

Scanner State
-------------

Tracks position and partial line across chunk boundaries.

> data ScanState = ScanState
>   { ssOffset       :: Offset          -- Current read position
>   , ssPartial      :: BS.ByteString   -- Partial line from previous chunk
>   , ssLines        :: [BS.ByteString] -- Accumulated lines (in scan order)
>   , ssLineCount    :: Int             -- Count of lines (avoids O(n) length calls)
>   , ssFileSize     :: Integer         -- Total file size
>   , ssEndsWithLF   :: Bool            -- True if file ends with newline
>   } deriving (Show)

Initialize scanner state based on direction.

> initScanState :: ScanStrategy -> Integer -> Bool -> ScanState
> initScanState strat fileSize endsWithLF =
>   let initialOffset = case stratPartialSide strat of
>                         LeftPartial  -> 0
>                         RightPartial -> fileSize
>   in ScanState
>        { ssOffset      = initialOffset
>        , ssPartial     = BS.empty
>        , ssLines       = []
>        , ssLineCount   = 0
>        , ssFileSize    = fileSize
>        , ssEndsWithLF  = endsWithLF
>        }

Core Scanning Logic
-------------------

Check if file ends with newline by reading last byte.

> checkFileEndsWithLF :: Integer
>                     -> (Offset -> Integer -> IO BS.ByteString)
>                     -> IO Bool
> checkFileEndsWithLF fileSize readFn
>   | fileSize == 0 = return True  -- Empty file is canonical
>   | otherwise = do
>       lastByte <- readFn (fileSize - 1) 1
>       return $ not (BS.null lastByte) && BS.head lastByte == lfByte

Scan lines in the given direction. This is the main API function.
Canonicalizes input by treating missing trailing newline as present.

> scanLines :: Direction                    -- ^ Scan direction
>           -> Integer                      -- ^ File size
>           -> (Offset -> Integer -> IO BS.ByteString)  -- ^ Read function
>           -> Int                          -- ^ Number of lines to collect
>           -> IO [T.Text]                  -- ^ Collected lines (in file order)
> scanLines dir fileSize readFn count = do
>   let strat = getStrategy dir
>   endsWithLF <- checkFileEndsWithLF fileSize readFn
>   let initialState = initScanState strat fileSize endsWithLF
>   finalState <- scanLoop strat readFn count initialState
>   let reachedEOF = not (stratHasMore strat finalState)  -- No more file data
>   let allLines = prepareFinalLines strat reachedEOF (ssEndsWithLF finalState) (ssPartial finalState) (ssLines finalState)
>   -- For backward, we want the LAST count lines; for forward, the FIRST count lines
>   let result = case dir of
>         Forward  -> take count allLines
>         Backward -> drop (max 0 (length allLines - count)) allLines
>   return result

Main scanning loop - now fully generic using strategy.

> scanLoop :: ScanStrategy
>          -> (Offset -> Integer -> IO BS.ByteString)
>          -> Int
>          -> ScanState
>          -> IO ScanState
> scanLoop strat readFn targetCount state
>   | ssLineCount state >= targetCount = return state
>   | not (stratHasMore strat state) = return state
>   | otherwise = do
>       let (readStart, readSize) = stratCalcRead strat defaultChunkSize state
>       chunk <- readFn readStart readSize
>       let newState = processChunk strat chunk state
>       scanLoop strat readFn targetCount newState

Process a chunk using strategy - now fully generic.
Assumes canonical format (as if file ends with newline).

> processChunk :: ScanStrategy -> BS.ByteString -> ScanState -> ScanState
> processChunk strat chunk state =
>   let -- Canonicalize chunk if it's the last/first chunk
>       canonicalChunk = stratCanonicalizeChunk strat chunk state
>       rawPieces = map stripCR $ BS.split lfByte canonicalChunk
>       -- Order pieces (reverse for backward)
>       pieces = stratOrderPieces strat rawPieces
>       (newLines, newPartial) = extractLinesCanonical strat pieces (ssPartial state)
>       offsetDelta = fromIntegral (BS.length chunk)  -- Use original chunk length
>       newOffset = stratUpdateOffset strat offsetDelta (ssOffset state)
>   in state { ssOffset = newOffset
>            , ssPartial = newPartial
>            , ssLines = stratCombineLines strat (ssLines state) newLines
>            , ssLineCount = ssLineCount state + length newLines
>            }

Strip trailing CR to handle both Unix (LF) and Windows (CRLF) line endings.

> stripCR :: BS.ByteString -> BS.ByteString
> stripCR bs
>   | BS.null bs = bs
>   | BS.last bs == 13 = BS.init bs  -- 13 is '\r'
>   | otherwise = bs

Extract lines from canonicalized chunks - now fully generic using strategy.
Assumes chunk format after split: [piece0, piece1, ..., pieceN, ""]
The last piece is empty because canonical chunks end with LF.

> extractLinesCanonical :: ScanStrategy
>                       -> [BS.ByteString]   -- ^ Pieces after split on LF
>                       -> BS.ByteString     -- ^ Partial from previous chunk
>                       -> ([BS.ByteString], BS.ByteString)  -- ^ (Lines, new partial)
> extractLinesCanonical _strat [] partial = ([], partial)  -- Empty chunk
> extractLinesCanonical strat pieces partial =
>   let edgePiece = stratGetEdgePiece strat pieces
>       edgeLine = stratCombinePartial strat partial edgePiece
>       middleLines = stratGetMiddle strat pieces
>       allLines = case stratPartialSide strat of
>                    LeftPartial  -> edgeLine : middleLines
>                    RightPartial -> middleLines ++ [edgeLine]
>       newPartial = stratGetNewPartial strat pieces
>   in (allLines, newPartial)

Prepare final result with proper ordering and decoding.
Prepare final result with proper ordering and decoding.
In canonical format, partial should be empty at end.

> prepareFinalLines :: ScanStrategy -> Bool -> Bool -> BS.ByteString -> [BS.ByteString] -> [T.Text]
> prepareFinalLines strat reachedEOF _endsWithLF partial lns =
>   let -- Add partial line if it exists AND we reached EOF
>       allLines = if BS.null partial || not reachedEOF
>                    then lns
>                    else case stratPartialSide strat of
>                           LeftPartial  -> lns ++ [partial]
>                           RightPartial -> partial : lns
>       decoded = map (normalizeLine . decodeUtf8Lenient) allLines
>   in stratFinalOrder strat decoded
