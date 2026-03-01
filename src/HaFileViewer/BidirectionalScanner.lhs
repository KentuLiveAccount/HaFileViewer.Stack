Bidirectional Scanner - Symmetric Forward/Backward Line Scanning
================================================================

This module implements efficient line-by-line scanning in both directions
using a strategy pattern to capture direction-dependent operations.

Key Design Principles:

1. **Canonical Format**: All files are treated as if they end with a newline (LF).
   Non-canonical files have LF added virtually during processing.

2. **Chunk-based Reading**: Files are read in chunks to handle arbitrarily large files.
   Incomplete lines at chunk boundaries are carried over as "partials".

3. **Strategy Pattern**: All direction-dependent logic is encapsulated in ScanStrategy,
   making the symmetric and asymmetric aspects explicit.

4. **Output Symmetry**: Both directions return lines in file order:
   - Forward: first N lines
   - Backward: last N lines
   This requires strategic asymmetry in processing (e.g., reversing middle pieces).

5. **Platform Independence**: Handles both Unix (LF) and Windows (CRLF) line endings
   via stripCR function.

Processing Pipeline:
  1. Read chunk from file
  2. Canonicalize (ensure ends with LF)
  3. Split on LF → pieces
  4. Order pieces (forward: id, backward: reverse)
  5. Extract lines from pieces (edge + middle + partial)
  6. Accumulate lines across chunks
  7. Final ordering and decoding

Module Header
-------------

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RankNTypes #-}
> module HaFileViewer.BidirectionalScanner
>   ( Direction(..)
>   , scanLines
>   , scanLinesWithOffsets
>   , ChunkSize
>   , defaultChunkSize
>   ) where
>
> import qualified Data.ByteString as BS
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as TE
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

> combinePartial :: BS.ByteString -> BS.ByteString -> BS.ByteString
> combinePartial partial piece = BS.append partial piece

> getEdgePiece :: [BS.ByteString] -> BS.ByteString
> getEdgePiece ps = if null ps then BS.empty else head ps

> getNewPartial :: [BS.ByteString] -> BS.ByteString
> getNewPartial ps = if null ps then BS.empty else last ps

ScanStrategy encapsulates all direction-dependent operations.
Only functions that differ between forward and backward are included.

> data ScanStrategy = ScanStrategy
>   { -- ** Loop control
>     stratHasMore           :: ScanState -> Bool
>     -- ^ Determines if there's more data to read from file.
>     -- Forward: offset < fileSize (reading left-to-right)
>     -- Backward: offset > 0 (reading right-to-left)
>   
>   , stratCalcRead          :: ChunkSize -> ScanState -> (Offset, Integer)
>     -- ^ Calculates where to read next chunk and how much.
>     -- Forward: reads from current offset forward
>     -- Backward: reads from (offset - chunkSize) to offset
>   
>   , stratUpdateOffset      :: Integer -> Offset -> Offset
>     -- ^ Updates file position after reading a chunk.
>     -- Forward: offset + delta (move right)
>     -- Backward: offset - delta (move left)
>   
>     -- ** Line accumulation
>   , stratCombineLines      :: [BS.ByteString] -> [BS.ByteString] -> [BS.ByteString]
>     -- ^ Combines accumulated lines with newly extracted lines.
>     -- Forward: (++) appends to end (chronological order)
>     -- Backward: flip (++) prepends to beginning (reverse chronological)
>   
>   , stratPartialSide       :: PartialSide
>     -- ^ Which edge of chunk carries the partial line.
>     -- Forward: LeftPartial (incomplete line at left edge continues from prev chunk)
>     -- Backward: RightPartial (after reversal, partial is logically on right)
>   
>   , stratFinalOrder        :: forall a. [a] -> [a]
>     -- ^ Final transformation to ensure file order output.
>     -- Forward: id (already in file order)
>     -- Backward: id (reversal of middle pieces ensures file order)
>   
>     -- ** Chunk processing
>   , stratCanonicalizeChunk :: BS.ByteString -> ScanState -> BS.ByteString
>     -- ^ Ensures chunk ends with LF for canonical processing.
>     -- Adds LF to last chunk if file doesn't end with newline.
>   
>   , stratOrderPieces       :: [BS.ByteString] -> [BS.ByteString]
>     -- ^ Orders pieces after BS.split for extraction.
>     -- Forward: id (keep file order)
>     -- Backward: reverse and drop trailing empty (process right-to-left)
>   
>     -- ** Line extraction (operates on ordered pieces)
>   , stratGetMiddle         :: [BS.ByteString] -> [BS.ByteString]
>     -- ^ Extracts complete lines from middle of pieces.
>     -- Forward: extractMiddlePieces (drop first and last)
>     -- Backward: reverse . extractMiddlePieces (restore file order)
>     --   Note: This reverse compensates for stratOrderPieces' reverse
>     -- Common functions (used by both): combinePartial, getEdgePiece, getNewPartial
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
>   , stratGetMiddle = extractMiddlePieces       -- No transformation needed
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
>   , stratGetMiddle = reverse . extractMiddlePieces  -- Reverse middle to restore file order!
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
>   , ssLineOffsets  :: [Offset]        -- Byte offsets (parallel to ssLines)
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
>        , ssLineOffsets = []
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

New API: scanLines with offset tracking
----------------------------------------

Identical to scanLines but returns byte offsets for each line.

> scanLinesWithOffsets :: Direction                    -- ^ Scan direction
>                      -> Integer                      -- ^ File size
>                      -> (Offset -> Integer -> IO BS.ByteString)  -- ^ Read function
>                      -> Int                          -- ^ Number of lines to collect
>                      -> IO [(T.Text, Offset)]        -- ^ Collected lines with offsets
> scanLinesWithOffsets dir fileSize readFn count = do
>   let strat = getStrategy dir
>   endsWithLF <- checkFileEndsWithLF fileSize readFn
>   let initialState = initScanState strat fileSize endsWithLF
>   finalState <- scanLoopWithOffsets strat readFn count initialState
>   let reachedEOF = not (stratHasMore strat finalState)
>   let allLinesWithOffsets = prepareFinalLinesWithOffsets strat reachedEOF (ssEndsWithLF finalState) (ssPartial finalState) (ssLines finalState)
>   -- For backward, we want the LAST count lines; for forward, the FIRST count lines
>   let result = case dir of
>         Forward  -> take count allLinesWithOffsets
>         Backward -> drop (max 0 (length allLinesWithOffsets - count)) allLinesWithOffsets
>   return result

Scanning loop that tracks offsets for each line.

> scanLoopWithOffsets :: ScanStrategy
>                     -> (Offset -> Integer -> IO BS.ByteString)
>                     -> Int
>                     -> ScanState
>                     -> IO ScanState
> scanLoopWithOffsets strat readFn targetCount state
>   | ssLineCount state >= targetCount = return state
>   | not (stratHasMore strat state) = return state
>   | otherwise = do
>       let (readStart, readSize) = stratCalcRead strat defaultChunkSize state
>       chunk <- readFn readStart readSize
>       let newState = processChunk strat chunk state
>       scanLoopWithOffsets strat readFn targetCount newState

Helper to prepare final lines with their byte offsets.
For now, we calculate offsets by summing line lengths.
TODO: Track offsets more efficiently during scanning.

> prepareFinalLinesWithOffsets :: ScanStrategy -> Bool -> Bool -> BS.ByteString -> [BS.ByteString] -> [(T.Text, Offset)]
> prepareFinalLinesWithOffsets strat reachedEOF endsWithLF partial rawLines =
>   let -- First get the lines using existing logic
>       finalLines = prepareFinalLines strat reachedEOF endsWithLF partial rawLines
>       -- Then calculate offsets by summing lengths
>       -- This assumes forward direction starts at offset 0
>       -- For backward, offsets need adjustment based on actual positions
>       go _ [] = []
>       go currentOffset (line:rest) =
>         let lineBS = TE.encodeUtf8 line
>             lineLen = fromIntegral (BS.length lineBS) + 1  -- +1 for newline
>         in (line, currentOffset) : go (currentOffset + lineLen) rest
>   in go 0 finalLines

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
>            , ssLineOffsets = ssLineOffsets state  -- Will be updated in later steps
>            , ssLineCount = ssLineCount state + length newLines
>            }

Strip trailing CR to handle both Unix (LF) and Windows (CRLF) line endings.

> stripCR :: BS.ByteString -> BS.ByteString
> stripCR bs
>   | BS.null bs = bs
>   | BS.last bs == 13 = BS.init bs  -- 13 is '\r'
>   | otherwise = bs

Extract lines from canonicalized chunks - uses common functions and strategy.
Assumes chunk format after split: [piece0, piece1, ..., pieceN, ""]
The last piece is empty because canonical chunks end with LF.

> extractLinesCanonical :: ScanStrategy
>                       -> [BS.ByteString]   -- ^ Pieces after split on LF
>                       -> BS.ByteString     -- ^ Partial from previous chunk
>                       -> ([BS.ByteString], BS.ByteString)  -- ^ (Lines, new partial)
> extractLinesCanonical _strat [] partial = ([], partial)  -- Empty chunk
> extractLinesCanonical strat pieces partial =
>   let edgePiece = getEdgePiece pieces
>       edgeLine = combinePartial partial edgePiece
>       middleLines = stratGetMiddle strat pieces
>       allLines = case stratPartialSide strat of
>                    LeftPartial  -> edgeLine : middleLines
>                    RightPartial -> middleLines ++ [edgeLine]
>       newPartial = getNewPartial pieces
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
