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

6. **Offset Tracking**: scanLinesWithOffsets tracks byte offsets DURING scanning,
   not after. This is critical because:
   - Offsets are lost during BS.split operations
   - Partial lines carry offsets between chunks
   - UTF-8 re-encoding would give wrong byte counts
   - Backward scans must calculate offsets from chunk start, not end

Processing Pipeline:
  1. Read chunk from file
  2. Canonicalize (ensure ends with LF)
  3. Split on LF → pieces
  4. Calculate piece offsets (byte position of each piece in file)
  5. Order pieces (forward: id, backward: reverse)
  6. Extract lines from pieces (edge + middle + partial)
  7. Track offsets parallel to lines
  8. Accumulate lines across chunks
  9. Final ordering and decoding

Module Header
-------------

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RankNTypes #-}
> module HaFileViewer.Backend.BidirectionalScanner
>   ( Direction(..)
>   , scanLinesWithOffsets
>   , ChunkSize
>   , defaultChunkSize
>   ) where
>
> import qualified Data.ByteString as BS
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as TE
> import Data.Word (Word8)
> import HaFileViewer.Backend.Types
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

OFFSET TRACKING INVARIANTS:
- ssLineOffsets is parallel to ssLines (same length, corresponding positions)
- All offsets are in file order (monotonically increasing), even for backward scans
- ssPartialOffset tracks where the partial line started in the file
- When combining partial + edgePiece, ALWAYS use partial's offset (not edge's)

> data ScanState = ScanState
>   { ssOffset       :: Offset          -- Current read position
>   , ssPartial      :: BS.ByteString   -- Partial line from previous chunk
>   , ssPartialOffset :: Offset         -- Byte offset of partial line START
>   , ssLines        :: [BS.ByteString] -- Accumulated lines (in scan order)
>   , ssLineOffsets  :: [Offset]        -- Byte offsets (parallel to ssLines, file order)
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
>        , ssPartialOffset = initialOffset
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

Scan lines in the given direction, returning byte offsets for each line
plus an end offset indicating where the next line would start.
Canonicalizes input by treating missing trailing newline as present.

> scanLinesWithOffsets :: Direction                    -- ^ Scan direction
>                      -> Integer                      -- ^ File size
>                      -> (Offset -> Integer -> IO BS.ByteString)  -- ^ Read function
>                      -> Int                          -- ^ Number of lines to collect
>                      -> IO ([(T.Text, Offset)], Offset)  -- ^ (lines with offsets, end offset)
> scanLinesWithOffsets dir fileSize readFn count = do
>   let strat = getStrategy dir
>   endsWithLF <- checkFileEndsWithLF fileSize readFn
>   let initialState = initScanState strat fileSize endsWithLF
>   finalState <- scanLoopWithOffsets strat readFn count initialState
>   let reachedEOF = not (stratHasMore strat finalState)
>   let allLinesWithOffsets = prepareFinalLinesWithOffsets strat reachedEOF (ssEndsWithLF finalState) (ssPartial finalState) (ssPartialOffset finalState) (ssLines finalState) (ssLineOffsets finalState)
>   let (result, endOffset) = case dir of
>         Forward ->
>           let (taken, rest) = splitAt count allLinesWithOffsets
>               endOff = case rest of
>                          ((_, off):_) -> off
>                          []           -> fileSize
>           in (taken, endOff)
>         Backward ->
>           let dropped = drop (max 0 (length allLinesWithOffsets - count)) allLinesWithOffsets
>           in (dropped, fileSize)
>   return (result, endOffset)

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
Now uses stored offsets directly instead of recalculating.

WHY NOT RECALCULATE?
Previously, we recalculated offsets by encoding Text back to ByteString.
This was WRONG because:
1. UTF-8 encoding might differ from original bytes (normalization)
2. Partial line offsets are lost (no way to know where partial started)
3. Accumulates rounding errors across chunks

Solution: Store offsets DURING scanning, use them here without modification.

> prepareFinalLinesWithOffsets :: ScanStrategy -> Bool -> Bool -> BS.ByteString -> Offset -> [BS.ByteString] -> [Offset] -> [(T.Text, Offset)]
> prepareFinalLinesWithOffsets strat reachedEOF endsWithLF partial partialOffset rawLines rawOffsets =
>   let -- First get the lines using existing logic
>       finalLines = prepareFinalLines strat reachedEOF endsWithLF partial rawLines
>       -- Prepare final offsets matching the final lines
>       finalOffsets = if BS.null partial || not reachedEOF
>                      then rawOffsets
>                      else case stratPartialSide strat of
>                             LeftPartial  -> rawOffsets ++ [partialOffset]
>                             RightPartial -> partialOffset : rawOffsets
>   in zip finalLines finalOffsets

Calculatebyte offset for the start of each piece after splitting on LF.
Each piece is separated by LF (1 byte), so offsets account for these delimiters.

CRITICAL: startOffset must be the byte position where the chunk STARTS in the file.
For backward scans: ssOffset points to chunk END after reading, so subtract chunk size!

> calculatePieceOffsets :: Offset -> [BS.ByteString] -> [Offset]
> calculatePieceOffsets _startOffset [] = []
> calculatePieceOffsets startOffset pieces =
>   let go _ [] = []
>       go currentOffset (piece:rest) =
>         -- BS.split excludes the delimiter (LF), but includes everything else (including CR)
>         -- So piece length already includes CR if present
>         -- We just need to add 1 for the excluded LF
>         let nextOffset = currentOffset + fromIntegral (BS.length piece) + 1  -- +1 for LF
>         in currentOffset : go nextOffset rest
>   in go startOffset pieces

Process a chunk using strategy - now fully generic.
Assumes canonical format (as if file ends with newline).

OFFSET TRACKING CRITICAL BUG FIX:
For backward scans, after reading a chunk, ssOffset points to where the read STARTED
(the chunk END in file). To calculate piece offsets, we need the chunk START.
Solution: Subtract offsetDelta to get chunk start position.

This was the most subtle bug in the implementation - backward scans would have
all offsets wrong by chunk size without this correction.

> processChunk :: ScanStrategy -> BS.ByteString -> ScanState -> ScanState
> processChunk strat chunk state =
>   let -- Canonicalize chunk if it's the last/first chunk
>       canonicalChunk = stratCanonicalizeChunk strat chunk state
>       rawPieces = map stripCR $ BS.split lfByte canonicalChunk
>       offsetDelta = fromIntegral (BS.length chunk)  -- Use original chunk length
>       -- Calculate byte offset where this chunk STARTS in the file
>       -- CRITICAL: For backward, ssOffset is chunk END, must subtract to get START
>       chunkStartOffset = case stratPartialSide strat of
>                            LeftPartial  -> ssOffset state  -- Forward: ssOffset is chunk start
>                            RightPartial -> ssOffset state - offsetDelta  -- Backward: subtract to get start
>       -- Calculate byte offsets for each raw piece (from chunk start)
>       -- Note: pieces are BEFORE stripCR at this point (they still have CR if present)
>       piecesBeforeStrip = BS.split lfByte canonicalChunk  -- Split before stripping
>       rawPieceOffsets = calculatePieceOffsets chunkStartOffset piecesBeforeStrip
>       -- Order pieces (reverse for backward)
>       pieces = stratOrderPieces strat rawPieces
>       -- Apply same ordering to offsets manually (reverse for backward, drop trailing if needed)
>       pieceOffsets = case stratPartialSide strat of
>                        LeftPartial  -> rawPieceOffsets  -- Forward: keep as is
>                        RightPartial ->  -- Backward: reverse and drop trailing
>                          let reversed = reverse rawPieceOffsets
>                              dropEmpty ps = if null ps then ps else tail ps  -- Drop last (which was first)
>                          in if null rawPieces || not (BS.null (last rawPieces))
>                             then reversed
>                             else dropEmpty reversed
>       (newLines, newLineOffsets, newPartial, newPartialOffset) = 
>         extractLinesCanonical strat pieceOffsets pieces (ssPartial state) (ssPartialOffset state)
>       newOffset = stratUpdateOffset strat offsetDelta (ssOffset state)
>       -- Combine offsets using the same logic as stratCombineLines
>       combinedOffsets = case stratPartialSide strat of
>                           LeftPartial  -> ssLineOffsets state ++ newLineOffsets  -- Forward: append
>                           RightPartial -> newLineOffsets ++ ssLineOffsets state  -- Backward: prepend
>   in state { ssOffset = newOffset
>            , ssPartial = newPartial
>            , ssPartialOffset = newPartialOffset
>            , ssLines = stratCombineLines strat (ssLines state) newLines
>            , ssLineOffsets = combinedOffsets
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
Now tracks byte offsets parallel to lines.

PARTIAL OFFSET PRESERVATION (Critical Invariant):
When a line spans multiple chunks, the FIRST chunk determines its offset.
Example: "hello" at offset 100, next chunk starts "world\n"
  - Combined line "helloworld" must have offset 100 (not offset of "world")
  - This is why we use partialOffset, not edgeOffset!

OFFSET ORDERING INVARIANT:
All returned offsets MUST be in file order (monotonically increasing).
For backward scans, pieces are reversed but offsets are adjusted to maintain file order.

> extractLinesCanonical :: ScanStrategy
>                       -> [Offset]          -- ^ Byte offsets for each piece
>                       -> [BS.ByteString]   -- ^ Pieces after split on LF
>                       -> BS.ByteString     -- ^ Partial from previous chunk
>                       -> Offset            -- ^ Offset of partial line START
>                       -> ([BS.ByteString], [Offset], BS.ByteString, Offset)  -- ^ (Lines, offsets, new partial, partial offset)
> extractLinesCanonical _strat _ [] partial partialOffset = ([], [], partial, partialOffset)  -- Empty chunk
> extractLinesCanonical strat pieceOffsets pieces partial partialOffset =
>   let edgePiece = getEdgePiece pieces
>       edgeOffset = if null pieceOffsets then partialOffset else head pieceOffsets
>       -- CRITICAL: When combining partial + edge, use partial's offset (not edge's)
>       -- The partial started earlier in the file!
>       -- EXCEPTION: If partial is empty, use edge's offset
>       edgeLine = combinePartial partial edgePiece
>       edgeLineOffset = if BS.null partial then edgeOffset else partialOffset
>       middleLines = stratGetMiddle strat pieces
>       -- Get offsets for middle pieces (apply same transformation as for lines)
>       middleOffsets = if null pieceOffsets || length pieceOffsets < 2
>                       then []
>                       else case stratPartialSide strat of
>                              LeftPartial  -> tail (init pieceOffsets)
>                              RightPartial -> reverse (tail (init pieceOffsets))  -- Same as: reverse . extractMiddlePieces
>       allLines = case stratPartialSide strat of
>                    LeftPartial  -> edgeLine : middleLines
>                    RightPartial -> middleLines ++ [edgeLine]
>       allOffsets = case stratPartialSide strat of
>                      LeftPartial  -> edgeLineOffset : middleOffsets
>                      RightPartial -> middleOffsets ++ [edgeLineOffset]
>       newPartial = getNewPartial pieces
>       newPartialOffset = if null pieceOffsets then partialOffset else last pieceOffsets
>   in (allLines, allOffsets, newPartial, newPartialOffset)

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
