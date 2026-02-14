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

> data ScanStrategy = ScanStrategy
>   { stratHasMore           :: ScanState -> Bool
>   , stratCalcRead          :: ChunkSize -> ScanState -> (Offset, Integer)
>   , stratUpdateOffset      :: Integer -> Offset -> Offset
>   , stratCombineLines      :: [BS.ByteString] -> [BS.ByteString] -> [BS.ByteString]
>   , stratPartialSide       :: PartialSide
>   , stratFinalOrder        :: forall a. [a] -> [a]
>   , stratCanonicalizeChunk :: BS.ByteString -> ScanState -> BS.ByteString
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
>   , stratCombinePartial = \partial piece -> BS.append partial piece  -- partial on left
>   , stratGetEdgePiece = head                    -- first piece combines with partial
>   , stratGetMiddle = tail . init                -- drop first and last
>   , stratGetNewPartial = last                   -- last piece is new partial
>   }

Create strategy for backward scanning.

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
>   , stratPartialSide = RightPartial
>   , stratFinalOrder = reverse
>   , stratCanonicalizeChunk = \chunk state ->
>       let isEOFChunk = ssOffset state >= ssFileSize state
>           needsLF = isEOFChunk && not (ssEndsWithLF state) && not (BS.null chunk)
>       in if needsLF then BS.snoc chunk lfByte else chunk
>   , stratCombinePartial = \piece partial -> BS.append piece partial  -- partial on right
>   , stratGetEdgePiece = last . init             -- last real piece combines with partial
>   , stratGetMiddle = init . tail . init         -- drop first, trailing empty, then last real
>   , stratGetNewPartial = head                   -- first piece is new partial
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
>   return $ prepareFinalLines strat (ssEndsWithLF finalState) (ssPartial finalState) (ssLines finalState)

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
>       pieces = BS.split lfByte canonicalChunk
>       (newLines, newPartial) = extractLinesCanonical strat pieces (ssPartial state)
>       offsetDelta = fromIntegral (BS.length chunk)  -- Use original chunk length
>       newOffset = stratUpdateOffset strat offsetDelta (ssOffset state)
>   in state { ssOffset = newOffset
>            , ssPartial = newPartial
>            , ssLines = stratCombineLines strat (ssLines state) newLines
>            , ssLineCount = ssLineCount state + length newLines
>            }

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
>       allLines = if BS.null edgeLine then middleLines else 
>                    case stratPartialSide strat of
>                      LeftPartial  -> edgeLine : middleLines
>                      RightPartial -> middleLines ++ [edgeLine]
>       newPartial = stratGetNewPartial strat pieces
>   in (allLines, newPartial)

Prepare final result with proper ordering and decoding.
In canonical format, partial should be empty at end.

> prepareFinalLines :: ScanStrategy -> Bool -> BS.ByteString -> [BS.ByteString] -> [T.Text]
> prepareFinalLines strat _endsWithLF partial lns =
>   let -- Add partial line if it exists (shouldn't happen in canonical format)
>       allLines = if BS.null partial
>                    then lns
>                    else case stratPartialSide strat of
>                           LeftPartial  -> lns ++ [partial]
>                           RightPartial -> partial : lns
>       decoded = map (normalizeLine . decodeUtf8Lenient) allLines
>   in stratFinalOrder strat decoded
