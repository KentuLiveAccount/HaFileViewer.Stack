LineCache: Primary Component for Efficient Line Access
=======================================================

This module provides the main line caching component for efficient
access to large files. It internally maintains both a sparse index
(for seeking) and a content cache (for repeated access).

Architecture Overview:

1. **Sparse Index**: Maps line numbers to byte offsets for fast seeking.
   Built incrementally as lines are scanned. Enables O(log n) random access.

2. **Content Cache**: LRU cache storing (startLine, count) → [lines].
   Evicts least recently used entries when full. Configured via CacheConfig.

3. **File Modification Tracking**: Checks file modification time on each access.
   Invalidates cache and rebuilds index if file changes.

4. **BidirectionalScanner Integration**: Uses scanLinesWithOffsets to get
   correct byte offsets during scanning (not recalculated afterward).

Performance Characteristics:
- Sequential access: O(1) with cache hits, O(n) on cache miss
- Random access: O(log n) with sparse index for seeking, then O(n) for scan
- Memory: O(cache size) for content + O(lines/granularity) for sparse index
- Large files (100MB+): Fast random access, bounded memory usage

Offset Source-of-Truth Principle
--------------------------------

**Never manufacture byte offsets.**  Every byte offset used anywhere in
this module — for keying the cache, indexing the sparse index, seeking
in the file, or linking entries — must originate from the scanner.

The scanner is the single place that knows how to translate raw file
bytes into line-start offsets.  It handles line-ending size (LF vs CRLF),
UTF-8 byte counting, partial lines across chunk boundaries, and other
encoding details that are surprisingly easy to get wrong elsewhere.

Concretely, this means:

- **Get offsets from ``scanLinesWithOffsets``**, which returns
  ``[(text, offset)]`` plus an ``endOffset``.  Use these values verbatim.

- **Do not compute** offsets by ``length text + 1``, by re-encoding
  ``Text`` to ``ByteString``, or by any other byte-counting arithmetic.
  These computations look correct but break on CRLF files, multi-byte
  UTF-8 characters, and partial-line boundaries.

- **Pass offsets through** when handing off to the scanner: if a piece of
  cache code holds an offset M and needs to scan from there, it calls the
  scanner with M unchanged.  Both sides will agree on what M means
  because M originally came from the scanner.

This principle keeps the off-by-one risk surface as small as possible: it
lives entirely inside the scanner, where it is tested directly, rather
than being scattered across every consumer that touches offsets.

Code-review heuristic: **any arithmetic on a byte offset in this module
is suspicious and worth scrutiny.**

> module HaFileViewer.Backend.LineCache
>   ( -- * Types
>     LineCache
>   , CacheConfig(..)
>   , CacheStats(..)
>   , LinePosition  -- Opaque type, constructor not exported
>   , ScanOrigin(..)  -- Export with constructors for pattern matching
>   , lpOrigin   -- Export accessor for origin
>   , emptyLinePosition  -- Sentinel position for empty-file initialization
>   , GetLinesResult(..)
>     
>     -- * Creation and lifecycle
>   , openLineCache
>   , openLineCacheWith
>   , closeLineCache
>   , withLineCache
>     
>     -- * New line-oriented API with positions
>   , getLinesFromStart
>   , getLinesFromEnd
>   , getLinesFrom
>     
>     -- * Cache management
>   , clearCache
>   , invalidateCache
>   , getCacheStats
>     
>     -- * Configuration
>   , defaultConfig
>     
>     -- * Pure helper functions (exported for testing)
>   , calculateForwardLineNumbers
>   , calculateBackwardLineNumbers
>   ) where

> import Prelude hiding (lookup)
> import qualified Data.Map.Strict as Map
> import qualified Data.Text as T
> import qualified Data.ByteString as BS
> import Data.IORef
> import Data.Time (UTCTime, getCurrentTime)
> import System.IO
> import System.Directory (getModificationTime, getFileSize)
> import Control.Exception (bracket, try, IOException)
> import Control.Monad (when, unless, forM_)
> 
> import HaFileViewer.Backend.BidirectionalScanner 
>   ( scanLinesWithOffsets, Direction(..) )
> import HaFileViewer.Backend.Types (Offset)
> import qualified HaFileViewer.Backend.SparseIndex as SI

> -- | Origin point for line numbering (where did we start?)
> data ScanOrigin = FromStart | FromEnd
>   deriving (Show, Eq)

> -- | Result of a cache line-fetch operation
> data GetLinesResult
>   = LinesLoaded [(Integer, T.Text)] LinePosition LinePosition
>   | AtBoundary
>   | LoadFailed String
>   deriving (Show, Eq)

Configuration
-------------

> -- | Configuration for line cache
> data CacheConfig = CacheConfig
>   { ccIndexStep    :: Int  -- ^ Sparse index granularity (default: 1024)
>   , ccMaxContent   :: Int  -- ^ Max lines to cache (default: 10000)
>   , ccChunkSize    :: Int  -- ^ Chunk size for scanning (default: 65536)
>   } deriving (Show, Eq)

> defaultConfig :: CacheConfig
> defaultConfig = CacheConfig
>   { ccIndexStep  = 1024
>   , ccMaxContent = 10000
>   , ccChunkSize  = 65536
>   }

Data Types
----------

> -- | Single cache entry: a line plus the byte offset where the *next* line
> -- begins.  ``ceNextOffset`` is always supplied by the scanner — either as
> -- the adjacent entry's offset, the scanner's ``endOffset`` (forward tail),
> -- or a caller-supplied scan bound (backward tail).  Never compute it by
> -- byte-counting ``ceText``.
> data CacheEntry = CacheEntry
>   { ceText       :: !T.Text
>   , ceNextOffset :: !Offset
>   } deriving (Show, Eq)

> -- | Build ``(offset, CacheEntry)`` pairs from an ascending-offset
> -- ``(text, offset)`` list plus the next-offset for the *last* entry.
> --
> -- The tail's next-offset cannot come from the entry list itself; the
> -- caller must supply it (scanner ``endOffset`` for forward tails, the
> -- scan bound for backward tails).  Inner entries get their next-offset
> -- from the adjacent entry — pure scanner-origin data, no arithmetic.
> buildCacheEntries :: [(T.Text, Offset)] -> Offset -> [(Offset, CacheEntry)]
> buildCacheEntries [] _ = []
> buildCacheEntries [(t, o)] tailNext = [(o, CacheEntry t tailNext)]
> buildCacheEntries ((t1, o1) : rest@((_, o2) : _)) tailNext =
>   (o1, CacheEntry t1 o2) : buildCacheEntries rest tailNext

> -- | Walk the cache forward from ``startOffset`` for at most ``count`` lines,
> -- chasing ``ceNextOffset`` at each step.  Returns the collected entries in
> -- ascending-offset (read) order and the offset where the next line would
> -- begin (i.e. the continuation offset, suitable for bottomOffset).
> --
> -- If the chain breaks before ``count`` is reached, the partial walk is
> -- returned along with the offset where the chain broke.  Callers that
> -- want only pure-hit semantics should check ``length result == count``.
> walkForwardCache :: Map.Map Offset CacheEntry -> Offset -> Int
>                  -> ([(T.Text, Offset)], Offset)
> walkForwardCache cache = go
>   where
>     go currentOffset n
>       | n <= 0 = ([], currentOffset)
>       | otherwise = case Map.lookup currentOffset cache of
>           Nothing -> ([], currentOffset)
>           Just (CacheEntry t next) ->
>             let (rest, finalOff) = go next (n - 1)
>             in  ((t, currentOffset) : rest, finalOff)

> -- | Walk the cache backward from ``boundOffset`` (exclusive upper bound)
> -- for at most ``count`` lines, using ``Map.lookupLT`` plus a contiguity
> -- check (``ceNextOffset`` of the predecessor must equal the current
> -- offset).  Returns lines in ascending-offset order (matching the
> -- scanner's backward result order) plus the offset of the lowest line
> -- found, which is the new topOffset.
> --
> -- If the chain breaks before ``count`` is reached, returns the partial
> -- walk plus the lowest offset reached so far (or ``boundOffset`` if
> -- nothing was found).  Pure-hit callers should compare lengths.
> walkBackwardCache :: Map.Map Offset CacheEntry -> Offset -> Int
>                   -> ([(T.Text, Offset)], Offset)
> walkBackwardCache cache boundOffset count = go boundOffset count []
>   where
>     -- Accumulator already holds collected lines in ascending order.
>     go currentBound n acc
>       | n <= 0 = (acc, currentBound)
>       | otherwise = case Map.lookupLT currentBound cache of
>           Nothing -> (acc, currentBound)
>           Just (prevOff, CacheEntry t next)
>             | next == currentBound ->
>                 go prevOff (n - 1) ((t, prevOff) : acc)
>             | otherwise -> (acc, currentBound)  -- chain broke

> -- | Line cache with integrated sparse index and content cache
> data LineCache = LineCache
>   { -- File information
>     lcFilePath      :: FilePath
>   , lcFileSize      :: Integer
>   , lcFileModTime   :: IORef UTCTime
>   , lcHandle        :: IORef (Maybe Handle)
>     
>     -- Sparse index (internal optimization)
>   , lcSparseIdx   :: IORef SI.SparseIndex
>   , lcIndexStep   :: Int
>     
>     -- Content cache (primary storage) - keyed by file offset
>   , lcContent     :: IORef (Map.Map Offset CacheEntry)
>   , lcLRUOrder    :: IORef [Offset]  -- Most recent last
>   , lcMaxContent  :: Int
>     
>     -- Total lines (cached once known)
>   , lcTotalLines  :: IORef (Maybe Integer)
>     
>     -- Frontier tracking for zone-meeting total-line detection
>   , lcForwardHighOff  :: IORef Offset    -- ^ Byte offset after last forward-scanned line
>   , lcFwdLineCount    :: IORef Int       -- ^ Lines scanned forward from BOF
>   , lcBackwardLowOff  :: IORef Offset    -- ^ Byte offset OF first backward line
>   , lcBwdLineCount    :: IORef Int       -- ^ Lines scanned backward from EOF
>     
>     -- Stats counters (cumulative across all fetches; reset on invalidate)
>   , lcContentHits     :: IORef Int       -- ^ Number of fetches fully served from cache
>   , lcContentMisses   :: IORef Int       -- ^ Number of fetches that fell through to scan
>   , lcTotalScanned    :: IORef Integer   -- ^ Total lines produced by the scanner
>     
>     -- Configuration
>   , lcConfig      :: CacheConfig
>   }

> -- | Cache statistics for monitoring
> data CacheStats = CacheStats
>   { csContentHits   :: Int     -- ^ Content cache hits
>   , csContentMisses :: Int     -- ^ Content cache misses  
>   , csContentSize   :: Int     -- ^ Current content cache size
>   , csSparseSize    :: Int     -- ^ Sparse index size
>   , csTotalScanned  :: Integer -- ^ Total lines scanned
>   } deriving (Show, Eq)

-- | Position within a file for line-oriented reading
-- 
-- Contains only the file offset and scan origin - no display state.
-- The cache layer is transparent file I/O; the viewer layer tracks display state.

> data LinePosition = LinePosition 
>   { lpOffset :: Offset        -- ^ Byte offset in file
>   , lpOrigin :: ScanOrigin    -- ^ Scan direction/origin
>   } deriving (Show, Eq)

> -- | A sentinel `LinePosition` at offset 0 with `FromStart` origin.
> --
> -- Intended for callers that need to construct a valid empty state when no
> -- real position exists yet (e.g. initializing a viewer on a zero-byte file
> -- or after a failed initial load).  Scroll operations should not rely on
> -- this position for actual navigation; they already guard on empty viewports.
> emptyLinePosition :: LinePosition
> emptyLinePosition = LinePosition 0 FromStart

Creation and Lifecycle
----------------------

> -- | Open a line cache for a file (uses default configuration)
> openLineCache :: FilePath -> IO LineCache
> openLineCache path = openLineCacheWith path defaultConfig

> -- | Open with custom configuration
> openLineCacheWith :: FilePath -> CacheConfig -> IO LineCache
> openLineCacheWith path config = do
>   size <- getFileSize path
>   modTime <- getModificationTime path
>   
>   modTimeRef <- newIORef modTime
>   handleRef <- newIORef Nothing
>   sparseIdx <- newIORef SI.empty
>   content <- newIORef Map.empty
>   lruOrder <- newIORef []
>   totalLines <- newIORef Nothing
>   forwardHighOff      <- newIORef 0
>   fwdLineCount <- newIORef 0
>   backwardLowOff <- newIORef (toInteger (maxBound :: Int))
>   bwdLineCount <- newIORef 0
>   contentHits <- newIORef 0
>   contentMisses <- newIORef 0
>   totalScanned <- newIORef 0
>   
>   return $ LineCache
>     { lcFilePath = path
>     , lcFileSize = size
>     , lcFileModTime = modTimeRef
>     , lcHandle = handleRef
>     , lcSparseIdx = sparseIdx
>     , lcIndexStep = ccIndexStep config
>     , lcContent = content
>     , lcLRUOrder = lruOrder
>     , lcMaxContent = ccMaxContent config
>     , lcTotalLines = totalLines
>     , lcForwardHighOff      = forwardHighOff
>     , lcFwdLineCount    = fwdLineCount
>     , lcBackwardLowOff  = backwardLowOff
>     , lcBwdLineCount    = bwdLineCount
>     , lcContentHits     = contentHits
>     , lcContentMisses   = contentMisses
>     , lcTotalScanned    = totalScanned
>     , lcConfig = config
>     }

> -- | Close cache and cleanup resources
> closeLineCache :: LineCache -> IO ()
> closeLineCache lc = do
>   -- Close file handle if open
>   mHandle <- readIORef (lcHandle lc)
>   case mHandle of
>     Just h  -> hClose h
>     Nothing -> return ()
>   
>   -- Clear all caches
>   writeIORef (lcSparseIdx lc) SI.empty
>   writeIORef (lcContent lc) Map.empty
>   writeIORef (lcLRUOrder lc) []
>   writeIORef (lcHandle lc) Nothing

> -- | Bracket pattern for resource safety
> withLineCache :: FilePath -> (LineCache -> IO a) -> IO a
> withLineCache path = bracket (openLineCache path) closeLineCache

Pure Helper Functions for New API
----------------------------------

These pure functions are used by getLinesFromStart, getLinesFromEnd, and getLinesFrom.
They are extracted for testability - see test_linecache_pure.hs for unit tests.

> -- | Calculate line numbers for forward reading
> -- Starting from a given line number, generate N sequential line numbers
> calculateForwardLineNumbers :: Integer -> Int -> [Integer]
> calculateForwardLineNumbers startLine count = 
>   take count [startLine..]

> -- | Calculate line numbers for backward reading (from end of file)
> -- If total lines is known, generates positive numbers [total-count+1 .. total]
> -- Otherwise generates negative numbers [-count .. -1]
> calculateBackwardLineNumbers :: Int -> Maybe Integer -> [Integer]
> calculateBackwardLineNumbers count _ | count <= 0 = []
> calculateBackwardLineNumbers count Nothing        = [negate (fromIntegral count) .. (-1)]
> calculateBackwardLineNumbers count (Just total)   = [total - fromIntegral count + 1 .. total]

New Line-Oriented API with Positions
-------------------------------------

These functions provide a bidirectional line scanning API that returns line numbers
along with content, and an opaque position marker for resuming reads.

> -- | Read N lines from start of file (forward)
> -- Returns lines with positive line numbers [1, 2, 3, ...] and TWO positions
> -- topPosition: for scrolling up (backward), bottomPosition: for scrolling down (forward)
> getLinesFromStart :: LineCache -> Int -> IO GetLinesResult
> getLinesFromStart lc count = do
>   res <- (try (getLinesFromStart' lc count) :: IO (Either IOException ([(Integer, T.Text)], LinePosition, LinePosition)))
>   case res of
>     Left err               -> return (LoadFailed (show err))
>     Right ([], _, _)       -> return AtBoundary
>     Right (ls, topPos, botPos) -> return (LinesLoaded ls topPos botPos)
>
> getLinesFromStart' :: LineCache -> Int
>                   -> IO ([(Integer, T.Text)], LinePosition, LinePosition)
> getLinesFromStart' lc count = do
>   -- Check if file modified
>   modified <- checkModified lc
>   when modified $ invalidateCache lc
>   
>   -- Kill switch: flip to False to disable the chain-walking read path.
>   let cacheReadEnabled = True

Pure-hit short-circuit: walk the cache from offset 0; if it covers all
``count`` lines, return without disk I/O.

>   cache <- readIORef (lcContent lc)
>   let (hits, hitTailOffset) =
>         if cacheReadEnabled then walkForwardCache cache 0 count else ([], 0)
>   if cacheReadEnabled && length hits == count && count > 0
>     then do
>       modifyIORef' (lcContentHits lc) (+ 1)
>       updateLRU lc (map snd hits)
>       let lineNumbers = calculateForwardLineNumbers 1 count
>           result = zip lineNumbers (map fst hits)
>           topPos    = LinePosition (snd (head hits)) FromStart
>           bottomPos = LinePosition hitTailOffset    FromStart
>       return (result, topPos, bottomPos)
>     else do
>       when cacheReadEnabled $ modifyIORef' (lcContentMisses lc) (+ 1)
>       getLinesFromStartScan lc count
>
> getLinesFromStartScan :: LineCache -> Int
>                       -> IO ([(Integer, T.Text)], LinePosition, LinePosition)
> getLinesFromStartScan lc count = do
>   -- Open file handle
>   h <- ensureHandle lc
>   
>   -- Scan from offset 0
>   let readFn offset size = do
>         hSeek h AbsoluteSeek (fromInteger offset)
>         BS.hGet h (fromInteger size)
>   
>   -- Use scanLinesWithOffsets to get lines with their byte offsets
>   (linesWithOffsets, endOffset) <- scanLinesWithOffsets Forward (lcFileSize lc) readFn count
>   modifyIORef' (lcTotalScanned lc) (+ fromIntegral (length linesWithOffsets))
>   
>   -- Forward results are ascending; tail's next-offset is scanner endOffset.
>   insertScannedLines lc linesWithOffsets endOffset
>   
>   -- Generate line numbers from raw results (caller sees all lines)
>   let lineNumbers = calculateForwardLineNumbers 1 (length linesWithOffsets)
>       result = zip lineNumbers (map fst linesWithOffsets)
>   
>   -- If raw scan returned fewer than requested, we've hit EOF — record total
>   when (length linesWithOffsets < count) $
>     writeIORef (lcTotalLines lc) (Just $ fromIntegral (length linesWithOffsets))
>   
>   -- Update sparse index with line number → offset mappings
>   let indexStep = lcIndexStep lc
>       indexEntries = [(lineNum, offset) | ((text, offset), lineNum) <- zip linesWithOffsets lineNumbers,
>                                            lineNum `mod` fromIntegral indexStep == 0]
>   sparseIdx <- readIORef (lcSparseIdx lc)
>   let sparseIdx' = SI.insertBatch indexEntries sparseIdx
>   writeIORef (lcSparseIdx lc) sparseIdx'
>   
>   -- Calculate TWO positions from raw results
>   let topOffset = if null linesWithOffsets then 0 else snd (head linesWithOffsets)
>       bottomOffset = if null linesWithOffsets then 0 else endOffset
>       topPos = LinePosition topOffset FromStart
>       bottomPos = LinePosition bottomOffset FromStart
>   
>   -- Update forward frontier using capped subset (non-overlapping zone only)
>   unless (null result) $ do
>     bwdLow <- readIORef (lcBackwardLowOff lc)
>     let cappedForFrontier = takeWhile (\(_, off) -> off < bwdLow) linesWithOffsets
>     unless (null cappedForFrontier) $ do
>       let cappedCount = length cappedForFrontier
>           cappedBottomOff = if cappedCount < length linesWithOffsets
>                             then snd (linesWithOffsets !! cappedCount)
>                             else endOffset
>       modifyIORef' (lcForwardHighOff lc) (max cappedBottomOff)
>       modifyIORef' (lcFwdLineCount lc) (max (length cappedForFrontier))
>     checkFrontierOverlap lc
>
>   return (result, topPos, bottomPos)

> -- | Read N lines from end of file (backward)
> -- Returns lines with negative line numbers [-N, -N+1, ..., -1] and TWO positions
> getLinesFromEnd :: LineCache -> Int -> IO GetLinesResult
> getLinesFromEnd lc count = do
>   res <- (try (getLinesFromEnd' lc count) :: IO (Either IOException ([(Integer, T.Text)], LinePosition, LinePosition)))
>   case res of
>     Left err               -> return (LoadFailed (show err))
>     Right ([], _, _)       -> return AtBoundary
>     Right (ls, topPos, botPos) -> return (LinesLoaded ls topPos botPos)
>
> getLinesFromEnd' :: LineCache -> Int
>                 -> IO ([(Integer, T.Text)], LinePosition, LinePosition)
> getLinesFromEnd' lc count = do
>   -- Check if file modified
>   modified <- checkModified lc
>   when modified $ invalidateCache lc
>   
>   let cacheReadEnabled = True
>       fileSize = lcFileSize lc

Pure-hit short-circuit: walk backward from fileSize for ``count``
contiguous cached lines (result is ascending).

>   cache <- readIORef (lcContent lc)
>   let (hits, _hitLowOffset) =
>         if cacheReadEnabled then walkBackwardCache cache fileSize count else ([], fileSize)
>   if cacheReadEnabled && length hits == count && count > 0
>     then do
>       modifyIORef' (lcContentHits lc) (+ 1)
>       updateLRU lc (map snd hits)
>       mTotal <- readIORef (lcTotalLines lc)
>       let lineNumbers = calculateBackwardLineNumbers count mTotal
>           result = zip lineNumbers (map fst hits)
>           topPos    = LinePosition (snd (head hits)) FromEnd
>           bottomPos = LinePosition fileSize          FromEnd
>       return (result, topPos, bottomPos)
>     else do
>       when cacheReadEnabled $ modifyIORef' (lcContentMisses lc) (+ 1)
>       getLinesFromEndScan lc count
>
> getLinesFromEndScan :: LineCache -> Int
>                     -> IO ([(Integer, T.Text)], LinePosition, LinePosition)
> getLinesFromEndScan lc count = do
>   -- Open file handle
>   h <- ensureHandle lc
>   
>   -- Scan backward from end of file
>   let readFn offset size = do
>         hSeek h AbsoluteSeek (fromInteger offset)
>         BS.hGet h (fromInteger size)
>   
>   -- Use scanLinesWithOffsets in backward mode
>   (linesWithOffsets, _scannerEndOffset) <- scanLinesWithOffsets Backward (lcFileSize lc) readFn count
>   modifyIORef' (lcTotalScanned lc) (+ fromIntegral (length linesWithOffsets))
>   
>   -- Backward over [0, fileSize): tail next-offset is fileSize (= scan bound).
>   insertScannedLines lc linesWithOffsets (lcFileSize lc)
>   
>   mTotal <- readIORef (lcTotalLines lc)
>   -- Generate line numbers from raw results (caller sees all lines)
>   let lineNumbers = calculateBackwardLineNumbers (length linesWithOffsets) mTotal
>       result = zip lineNumbers (map fst linesWithOffsets)
>   
>   -- Update sparse index with line number → offset mappings
>   let indexStep = lcIndexStep lc
>       indexEntries = [(lineNum, offset) | ((text, offset), lineNum) <- zip linesWithOffsets lineNumbers,
>                                            lineNum `mod` fromIntegral indexStep == 0]
>   sparseIdx <- readIORef (lcSparseIdx lc)
>   let sparseIdx' = SI.insertBatch indexEntries sparseIdx
>   writeIORef (lcSparseIdx lc) sparseIdx'
>   
>   -- Bottom is fileSize: scanner endOffset for Backward over [0, fileSize).
>   let fileSize = lcFileSize lc
>       topOffset = if null linesWithOffsets then fileSize else snd (head linesWithOffsets)
>       bottomOffset = fileSize
>       topPos = LinePosition topOffset FromEnd
>       bottomPos = LinePosition bottomOffset FromEnd
>
>   -- Update backward frontier using capped subset (non-overlapping zone only)
>   unless (null linesWithOffsets) $ do
>     fwdHigh <- readIORef (lcForwardHighOff lc)
>     let cappedForFrontier = dropWhile (\(_, off) -> off < fwdHigh) linesWithOffsets
>     unless (null cappedForFrontier) $ do
>       let cappedTopOffset = snd (head cappedForFrontier)
>       modifyIORef' (lcBwdLineCount lc) (max (length cappedForFrontier))
>       modifyIORef' (lcBackwardLowOff lc) (min cappedTopOffset)
>       when (cappedTopOffset == 0) $ do
>         bwdCnt <- readIORef (lcBwdLineCount lc)
>         writeIORef (lcTotalLines lc) (Just (fromIntegral bwdCnt))
>     checkFrontierOverlap lc
>
>   return (result, topPos, bottomPos)

> -- | Read N lines from a given position in specified direction
> -- The startLineNum parameter tells the cache what line number corresponds to the start position
> -- Returns lines with appropriate line numbers and TWO positions to continue
> getLinesFrom :: LineCache -> LinePosition -> Direction -> Int -> Integer
>              -> IO GetLinesResult
> getLinesFrom lc pos dir count startLineNum = do
>   res <- (try (getLinesFrom' lc pos dir count startLineNum) :: IO (Either IOException ([(Integer, T.Text)], LinePosition, LinePosition)))
>   case res of
>     Left err               -> return (LoadFailed (show err))
>     Right ([], _, _)       -> return AtBoundary
>     Right (ls, topPos, botPos) -> return (LinesLoaded ls topPos botPos)
>
> getLinesFrom' :: LineCache -> LinePosition -> Direction -> Int -> Integer
>               -> IO ([(Integer, T.Text)], LinePosition, LinePosition)
> getLinesFrom' lc pos@(LinePosition startOffset origin) dir count startLineNum = do
>   -- Check if file modified
>   modified <- checkModified lc
>   when modified $ invalidateCache lc
>   
>   let cacheReadEnabled = True
>   
>   -- Phase 1: pure-hit short-circuit.
>   cache <- readIORef (lcContent lc)
>   let (hits, hitContinuation) = case (cacheReadEnabled, dir) of
>         (False, _)        -> ([], startOffset)
>         (True, Forward)   -> walkForwardCache  cache startOffset count
>         (True, Backward)  -> walkBackwardCache cache startOffset count
>   if cacheReadEnabled && length hits == count && count > 0
>     then do
>       modifyIORef' (lcContentHits lc) (+ 1)
>       updateLRU lc (map snd hits)
>       mTotal <- readIORef (lcTotalLines lc)
>       let resolveLineNum n = case (mTotal, n < 0) of
>             (Just total, True) -> total + n + 1
>             _                  -> n
>           lineNumbers = case dir of
>             Forward  -> [startLineNum .. startLineNum + fromIntegral count - 1]
>             Backward -> let resolvedStart = resolveLineNum startLineNum
>                         in [resolvedStart - fromIntegral count + 1 .. resolvedStart]
>           result = zip lineNumbers (map fst hits)
>           topOffset = case dir of
>             Forward  -> startOffset
>             Backward -> snd (head hits)  -- lowest offset (hits are ascending)
>           bottomOffset = case dir of
>             Forward  -> hitContinuation  -- ceNextOffset of the last hit
>             Backward -> startOffset
>           topPos    = LinePosition topOffset    origin
>           bottomPos = LinePosition bottomOffset origin
>       return (result, topPos, bottomPos)
>     else do
>       when cacheReadEnabled $ modifyIORef' (lcContentMisses lc) (+ 1)
>       getLinesFromScan lc pos dir count startLineNum

Note: backward ``tailNextOffset``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For a backward scan in ``getLinesFromScan`` we scanned ``[0, startOffset)``,
so the highest-offset entry's next line begins at ``startOffset`` — the
caller-supplied bound.  The scanner's own ``endOffset`` for ``Backward`` is
always ``fileSize``, which would be wrong here whenever
``startOffset < fileSize``.  Forward is simpler: the scanner's
``endOffset`` (adjusted to absolute) is exactly the tail's next-offset.

> getLinesFromScan :: LineCache -> LinePosition -> Direction -> Int -> Integer
>                  -> IO ([(Integer, T.Text)], LinePosition, LinePosition)
> getLinesFromScan lc (LinePosition startOffset origin) dir count startLineNum = do
>   -- Open file handle
>   h <- ensureHandle lc
>   
>   -- Create read function starting from given offset
>   let readFn offset size = do
>         let absOffset = case dir of
>               Forward  -> startOffset + offset
>               Backward -> offset  -- Backward offsets are already absolute
>         hSeek h AbsoluteSeek (fromInteger absOffset)
>         BS.hGet h (fromInteger size)
>       remainingSize = case dir of
>         Forward  -> lcFileSize lc - startOffset
>         Backward -> startOffset  -- For backward, we read from 0 to startOffset
>   
>   -- Scan in the specified direction
>   (rawLinesWithOffsets, rawEndOffset) <- scanLinesWithOffsets dir remainingSize readFn count
>   modifyIORef' (lcTotalScanned lc) (+ fromIntegral (length rawLinesWithOffsets))
>   
>   -- Adjust offsets to be absolute and cache ALL raw lines by offset
>   let adjustedLines = case dir of
>         Forward  -> [(text, startOffset + off) | (text, off) <- rawLinesWithOffsets]
>         Backward -> rawLinesWithOffsets  -- Already absolute
>       adjustedEndOffset = case dir of
>         Forward  -> startOffset + rawEndOffset
>         Backward -> rawEndOffset

Tail next-offset: scanner endOffset (Forward) or caller bound (Backward).
See "Note: backward tailNextOffset" above.

>   let tailNextOffset = case dir of
>         Forward  -> adjustedEndOffset
>         Backward -> startOffset
>   insertScannedLines lc adjustedLines tailNextOffset
>   
>   -- Calculate line numbers from raw results (caller sees all lines)
>   mTotal <- readIORef (lcTotalLines lc)
>   let resolveLineNum n = case (mTotal, n < 0) of
>         (Just total, True) -> total + n + 1
>         _                  -> n
>       lineNumbers = case dir of
>         Forward  -> [startLineNum .. startLineNum + fromIntegral (length adjustedLines) - 1]
>         Backward -> let resolvedStart = resolveLineNum startLineNum
>                     in [resolvedStart - fromIntegral (length adjustedLines) + 1 .. resolvedStart]
>       texts = map fst adjustedLines
>       result = zip lineNumbers texts
>   
>   -- If forward scan returned fewer raw lines than requested, we've reached EOF
>   when (dir == Forward && length adjustedLines < count && not (null adjustedLines)) $
>     writeIORef (lcTotalLines lc) (Just $ startLineNum + fromIntegral (length adjustedLines) - 1)
>   
>   -- Update sparse index from raw results
>   let indexStep = lcIndexStep lc
>       indexEntries = [(lineNum, offset) | ((text, offset), lineNum) <- zip adjustedLines lineNumbers,
>                                            lineNum `mod` fromIntegral indexStep == 0]
>   sparseIdx <- readIORef (lcSparseIdx lc)
>   let sparseIdx' = SI.insertBatch indexEntries sparseIdx
>   writeIORef (lcSparseIdx lc) sparseIdx'
>   
>   -- Calculate TWO positions from raw results
>   let topOffset = case dir of
>         Forward  -> startOffset
>         Backward -> if null adjustedLines then startOffset else snd (head adjustedLines)
>       bottomOffset = case dir of
>         Forward  -> if null adjustedLines then startOffset else adjustedEndOffset
>         Backward -> startOffset
>       topPos = LinePosition topOffset origin
>       bottomPos = LinePosition bottomOffset origin
>   
>   -- Update forward frontier using capped subset (non-overlapping zone only)
>   unless (dir == Backward || null adjustedLines) $ do
>     bwdLow <- readIORef (lcBackwardLowOff lc)
>     let cappedForFrontier = takeWhile (\(_, off) -> off < bwdLow) adjustedLines
>     unless (null cappedForFrontier) $ do
>       let lastCappedLineNum = startLineNum + fromIntegral (length cappedForFrontier) - 1
>           cappedCount = length cappedForFrontier
>           cappedBottomOff = if cappedCount < length adjustedLines
>                             then snd (adjustedLines !! cappedCount)
>                             else adjustedEndOffset
>       modifyIORef' (lcForwardHighOff lc) (max cappedBottomOff)
>       modifyIORef' (lcFwdLineCount lc) (max (fromIntegral lastCappedLineNum))
>     checkFrontierOverlap lc
>   
>   -- Update backward frontier using capped subset (non-overlapping zone only)
>   unless (dir == Forward || null adjustedLines) $ do
>     fwdHigh <- readIORef (lcForwardHighOff lc)
>     let cappedForFrontier = dropWhile (\(_, off) -> off < fwdHigh) adjustedLines
>     unless (null cappedForFrontier) $ do
>       let cappedTopOffset = snd (head cappedForFrontier)
>       oldLow <- readIORef (lcBackwardLowOff lc)
>       when (cappedTopOffset < oldLow) $ do
>         let newLines = length (takeWhile (\(_, off) -> off < oldLow) cappedForFrontier)
>         modifyIORef' (lcBwdLineCount lc) (+ newLines)
>         writeIORef (lcBackwardLowOff lc) cappedTopOffset
>       when (cappedTopOffset == 0) $ do
>         bwdCnt <- readIORef (lcBwdLineCount lc)
>         writeIORef (lcTotalLines lc) (Just (fromIntegral bwdCnt))
>     checkFrontierOverlap lc
>   
>   return (result, topPos, bottomPos)

Cache Management
----------------

> -- | Clear content cache only (keeps sparse index for efficiency)
> clearCache :: LineCache -> IO ()
> clearCache lc = do
>   writeIORef (lcContent lc) Map.empty
>   writeIORef (lcLRUOrder lc) []

> -- | Invalidate ALL caches (after file modification detected)
> invalidateCache :: LineCache -> IO ()
> invalidateCache lc = do
>   writeIORef (lcSparseIdx lc) SI.empty
>   writeIORef (lcContent lc) Map.empty
>   writeIORef (lcLRUOrder lc) []
>   writeIORef (lcTotalLines lc) Nothing
>   writeIORef (lcForwardHighOff lc) 0
>   writeIORef (lcFwdLineCount lc) 0
>   writeIORef (lcBackwardLowOff lc) (toInteger (maxBound :: Int))
>   writeIORef (lcBwdLineCount lc) 0
>   writeIORef (lcContentHits lc) 0
>   writeIORef (lcContentMisses lc) 0
>   writeIORef (lcTotalScanned lc) 0

> -- | Get cache statistics (for monitoring/debugging)
> getCacheStats :: LineCache -> IO CacheStats
> getCacheStats lc = do
>   content <- readIORef (lcContent lc)
>   sparseIdx <- readIORef (lcSparseIdx lc)
>   hits <- readIORef (lcContentHits lc)
>   misses <- readIORef (lcContentMisses lc)
>   scanned <- readIORef (lcTotalScanned lc)
>   
>   return $ CacheStats
>     { csContentHits = hits
>     , csContentMisses = misses
>     , csContentSize = Map.size content
>     , csSparseSize = SI.size sparseIdx
>     , csTotalScanned = scanned
>     }

Internal Helper Functions
-------------------------

> -- | Check if file has been modified since cache was created
> checkModified :: LineCache -> IO Bool
> checkModified lc = do
>   oldTime <- readIORef (lcFileModTime lc)
>   newTime <- getModificationTime (lcFilePath lc)
>   return $ newTime > oldTime

> -- | Check if forward and backward frontiers have met; if so, compute and store total
> checkFrontierOverlap :: LineCache -> IO ()
> checkFrontierOverlap lc = do
>   mTotal <- readIORef (lcTotalLines lc)
>   case mTotal of
>     Just _  -> return ()  -- Already known
>     Nothing -> do
>       fwdHigh <- readIORef (lcForwardHighOff lc)
>       bwdLow  <- readIORef (lcBackwardLowOff lc)
>       bwdCnt  <- readIORef (lcBwdLineCount lc)
>       fwdCnt  <- readIORef (lcFwdLineCount lc)
>       when (bwdCnt > 0 && fwdCnt > 0 && fwdHigh >= bwdLow) $
>         writeIORef (lcTotalLines lc) (Just (fromIntegral fwdCnt + fromIntegral bwdCnt))



> -- | Update LRU order (move accessed offsets to end)
> updateLRU :: LineCache -> [Offset] -> IO ()
> updateLRU lc accessed = do
>   lru <- readIORef (lcLRUOrder lc)
>   let lru' = filter (`notElem` accessed) lru ++ accessed
>   writeIORef (lcLRUOrder lc) lru'





> -- | Ensure file handle is open
> ensureHandle :: LineCache -> IO Handle
> ensureHandle lc = do
>   mHandle <- readIORef (lcHandle lc)
>   case mHandle of
>     Just h -> return h
>     Nothing -> do
>       h <- openBinaryFile (lcFilePath lc) ReadMode
>       writeIORef (lcHandle lc) (Just h)
>       return h



> -- | Insert a single line into cache with LRU eviction (keyed by offset)
> insertWithEviction :: LineCache -> Offset -> CacheEntry -> IO ()
> insertWithEviction lc offset entry = do
>   cache <- readIORef (lcContent lc)
>   lru <- readIORef (lcLRUOrder lc)
>   
>   -- Check if we need to evict
>   let currentSize = Map.size cache
>       maxSize = lcMaxContent lc
>   
>   (cache', lru') <- if currentSize >= maxSize && offset `Map.notMember` cache
>     then do
>       -- Need to evict oldest entry
>       case lru of
>         (oldest:rest) -> do
>           let cache'' = Map.delete oldest cache
>           return (cache'', rest)
>         [] -> return (cache, lru)
>     else return (cache, lru)
>   
>   -- Insert new line by offset
>   let cache'' = Map.insert offset entry cache'
>       lru'' = filter (/= offset) lru' ++ [offset]
>   
>   writeIORef (lcContent lc) cache''
>   writeIORef (lcLRUOrder lc) lru''

> -- | Insert an ascending-offset list of scan results into the cache.
> -- The ``tailNextOffset`` is the next-offset for the highest-offset entry;
> -- it must be a scanner-origin value (e.g. scanner ``endOffset`` for
> -- forward scans, or the caller-supplied scan bound for backward scans).
> insertScannedLines :: LineCache -> [(T.Text, Offset)] -> Offset -> IO ()
> insertScannedLines lc linesAsc tailNextOffset =
>   forM_ (buildCacheEntries linesAsc tailNextOffset) $ \(off, entry) ->
>     insertWithEviction lc off entry

