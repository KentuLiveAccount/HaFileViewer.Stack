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

> module HaFileViewer.Backend.LineCache
>   ( -- * Types
>     LineCache
>   , CacheConfig(..)
>   , CacheStats(..)
>   , LinePosition  -- Opaque type, constructor not exported
>   , ScanOrigin(..)  -- Export with constructors for pattern matching
>   , lpOrigin   -- Export accessor for origin
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
>   , extractNewPosition
>   ) where

> import Prelude hiding (lookup)
> import qualified Data.Map.Strict as Map
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as TE
> import qualified Data.ByteString as BS
> import Data.IORef
> import Data.Time (UTCTime, getCurrentTime)
> import System.IO
> import System.Directory (getModificationTime, getFileSize)
> import Control.Exception (bracket, try, IOException)
> import Control.Monad (when, unless, forM_)
> 
> import HaFileViewer.Backend.BidirectionalScanner 
>   ( scanLines, scanLinesWithOffsets, Direction(..) )
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

> -- | Line cache with integrated sparse index and content cache
> data LineCache = LineCache
>   { -- File information
>     lcFilePath      :: FilePath
>   , lcFileSize      :: Integer
>   , lcFileModTime   :: IORef UTCTime
>   , lcHandle        :: IORef (Maybe Handle)
>   , lcLineEndingLen :: Int  -- ^ Line ending length: 2 for CR-LF, 1 for LF-only
>     
>     -- Sparse index (internal optimization)
>   , lcSparseIdx   :: IORef SI.SparseIndex
>   , lcIndexStep   :: Int
>     
>     -- Content cache (primary storage) - keyed by file offset
>   , lcContent     :: IORef (Map.Map Offset T.Text)
>   , lcLRUOrder    :: IORef [Offset]  -- Most recent last
>   , lcMaxContent  :: Int
>     
>     -- Total lines (cached once known)
>   , lcTotalLines  :: IORef (Maybe Integer)
>     
>     -- Frontier tracking for zone-meeting total-line detection
>   , lcForwardHighOff       :: IORef Offset                   -- ^ Byte offset after last forward-scanned line
>   , lcFwdLineCount    :: IORef Int       -- ^ Lines scanned forward from BOF
>   , lcBackwardLowOff  :: IORef Offset    -- ^ Byte offset OF first backward line
>   , lcBwdLineCount    :: IORef Int       -- ^ Lines scanned backward from EOF
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

Creation and Lifecycle
----------------------

> -- | Detect line ending style by checking first line in file
> -- Returns 2 for CR-LF (Windows), 1 for LF-only (Unix)
> detectLineEnding :: FilePath -> IO Int
> detectLineEnding path = do
>   h <- openFile path ReadMode
>   hSetBinaryMode h True  -- Read raw bytes
>   chunk <- BS.hGet h 1024  -- Read first 1KB
>   hClose h
>   
>   -- Look for first LF and check if preceded by CR
>   let lfPos = BS.elemIndex 10 chunk  -- Find LF byte
>   case lfPos of
>     Nothing -> return 1  -- No newline found, assume LF-only
>     Just 0  -> return 1  -- LF at start, no room for CR
>     Just pos -> 
>       let prevByte = BS.index chunk (pos - 1)
>       in if prevByte == 13  -- CR byte
>          then return 2      -- CR-LF style (Windows)
>          else return 1      -- LF-only style (Unix)

> -- | Open a line cache for a file (uses default configuration)
> openLineCache :: FilePath -> IO LineCache
> openLineCache path = openLineCacheWith path defaultConfig

> -- | Open with custom configuration
> openLineCacheWith :: FilePath -> CacheConfig -> IO LineCache
> openLineCacheWith path config = do
>   size <- getFileSize path
>   modTime <- getModificationTime path
>   
>   -- Detect line ending style by reading first chunk
>   lineEndingLen <- detectLineEnding path
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
>   
>   return $ LineCache
>     { lcFilePath = path
>     , lcFileSize = size
>     , lcFileModTime = modTimeRef
>     , lcHandle = handleRef
>     , lcLineEndingLen = lineEndingLen
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

> -- | Extract new position from scan results
> -- For Forward: take offset after last line
> -- For Backward: take offset of first line
> extractNewPosition :: [(T.Text, Offset)] -> Direction -> Offset
> extractNewPosition [] _ = 0  -- Empty result, stay at same position
> extractNewPosition results Forward = 
>   let (lastText, lastOffset) = last results
>       lastLineLength = fromIntegral $ BS.length $ TE.encodeUtf8 lastText
>   in lastOffset + lastLineLength + 1  -- +1 for newline character
> extractNewPosition results Backward = 
>   snd (head results)  -- First line's offset

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
>   -- Open file handle
>   h <- ensureHandle lc
>   
>   -- Scan from offset 0
>   let readFn offset size = do
>         hSeek h AbsoluteSeek (fromInteger offset)
>         BS.hGet h (fromInteger size)
>   
>   -- Use scanLinesWithOffsets to get lines with their byte offsets
>   linesWithOffsets <- scanLinesWithOffsets Forward (lcFileSize lc) readFn count
>   
>   -- Cache ALL raw lines by offset
>   forM_ linesWithOffsets $ \(text, offset) ->
>     insertWithEviction lc offset text
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
>       bottomOffset = if null linesWithOffsets 
>                      then 0 
>                      else let (lastText, lastOff) = last linesWithOffsets
>                               textLen = fromIntegral (BS.length $ TE.encodeUtf8 lastText)
>                               lineEndLen = fromIntegral (lcLineEndingLen lc)
>                           -- text + line_ending (CR-LF=2, LF=1)
>                           in lastOff + textLen + lineEndLen
>       topPos = LinePosition topOffset FromStart
>       bottomPos = LinePosition bottomOffset FromStart
>   
>   -- Update forward frontier using capped subset (non-overlapping zone only)
>   unless (null result) $ do
>     bwdLow <- readIORef (lcBackwardLowOff lc)
>     let cappedForFrontier = takeWhile (\(_, off) -> off < bwdLow) linesWithOffsets
>     unless (null cappedForFrontier) $ do
>       let cappedBottomOff = let (lastText, lastOff) = last cappedForFrontier
>                                 textLen = fromIntegral (BS.length $ TE.encodeUtf8 lastText)
>                                 lineEndLen = fromIntegral (lcLineEndingLen lc)
>                             in lastOff + textLen + lineEndLen
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
>   -- Open file handle
>   h <- ensureHandle lc
>   
>   -- Scan backward from end of file
>   let readFn offset size = do
>         hSeek h AbsoluteSeek (fromInteger offset)
>         BS.hGet h (fromInteger size)
>   
>   -- Use scanLinesWithOffsets in backward mode
>   linesWithOffsets <- scanLinesWithOffsets Backward (lcFileSize lc) readFn count
>   
>   -- Cache ALL raw lines by offset
>   forM_ linesWithOffsets $ \(text, offset) ->
>     insertWithEviction lc offset text
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
>   -- Calculate TWO positions from raw results
>   let fileSize = lcFileSize lc
>       topOffset = if null linesWithOffsets then fileSize else snd (head linesWithOffsets)
>       bottomOffset = if null linesWithOffsets
>                      then fileSize
>                      else let (lastText, lastOff) = last linesWithOffsets
>                               textLen = fromIntegral (BS.length $ TE.encodeUtf8 lastText)
>                               lineEndLen = fromIntegral (lcLineEndingLen lc)
>                           -- text + line_ending (CR-LF=2, LF=1)
>                           in lastOff + textLen + lineEndLen
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
> getLinesFrom' lc (LinePosition startOffset origin) dir count startLineNum = do
>   -- Check if file modified
>   modified <- checkModified lc
>   when modified $ invalidateCache lc
>   
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
>   linesWithOffsets <- scanLinesWithOffsets dir remainingSize readFn count
>   
>   -- Adjust offsets to be absolute and cache ALL raw lines by offset
>   let adjustedLines = case dir of
>         Forward  -> [(text, startOffset + off) | (text, off) <- linesWithOffsets]
>         Backward -> linesWithOffsets  -- Already absolute
>   
>   forM_ adjustedLines $ \(text, offset) ->
>     insertWithEviction lc offset text
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
>         Forward  -> if null adjustedLines 
>                     then startOffset
>                     else let (lastText, lastOff) = last adjustedLines
>                              textLen = fromIntegral (BS.length $ TE.encodeUtf8 lastText)
>                              lineEndLen = fromIntegral (lcLineEndingLen lc)
>                          -- text + line_ending (CR-LF=2, LF=1)
>                          in lastOff + textLen + lineEndLen
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
>           cappedBottomOff = let (lastText, lastOff) = last cappedForFrontier
>                                 textLen = fromIntegral (BS.length $ TE.encodeUtf8 lastText)
>                                 lineEndLen = fromIntegral (lcLineEndingLen lc)
>                             in lastOff + textLen + lineEndLen
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

> -- | Get cache statistics (for monitoring/debugging)
> getCacheStats :: LineCache -> IO CacheStats
> getCacheStats lc = do
>   content <- readIORef (lcContent lc)
>   sparseIdx <- readIORef (lcSparseIdx lc)
>   
>   return $ CacheStats
>     { csContentHits = 0    -- TODO: Track hits
>     , csContentMisses = 0  -- TODO: Track misses
>     , csContentSize = Map.size content
>     , csSparseSize = SI.size sparseIdx
>     , csTotalScanned = 0   -- TODO: Track scanned lines
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
> insertWithEviction :: LineCache -> Offset -> T.Text -> IO ()
> insertWithEviction lc offset content = do
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
>   let cache'' = Map.insert offset content cache'
>       lru'' = filter (/= offset) lru' ++ [offset]
>   
>   writeIORef (lcContent lc) cache''
>   writeIORef (lcLRUOrder lc) lru''

