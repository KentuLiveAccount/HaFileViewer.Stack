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
>     
>     -- * Creation and lifecycle
>   , openLineCache
>   , openLineCacheWith
>   , closeLineCache
>   , withLineCache
>     
>     -- * Query operations
>   , getLines
>   , getLine
>   , getTotalLines
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

> import Prelude hiding (lookup, getLine)
> import qualified Data.Map.Strict as Map
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as TE
> import qualified Data.ByteString as BS
> import Data.IORef
> import Data.Time (UTCTime, getCurrentTime)
> import System.IO hiding (getLine)
> import System.IO (hSeek, SeekMode(..))
> import System.Directory (getModificationTime, getFileSize)
> import Control.Exception (bracket)
> import Control.Monad (when, forM_)
> 
> import HaFileViewer.Backend.BidirectionalScanner 
>   ( scanLines, scanLinesWithOffsets, Direction(..) )
> import HaFileViewer.Backend.Types (Offset)
> import qualified HaFileViewer.Backend.SparseIndex as SI

> -- | Origin point for line numbering (where did we start?)
> data ScanOrigin = FromStart | FromEnd
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

Query Operations
----------------

> -- | Get lines starting at given position (main API)
> -- Handles all caching internally
> -- | DEPRECATED: Get N lines starting from a line number
> -- Use getLinesFromStart, getLinesFromEnd, or getLinesFrom instead
> getLines :: LineCache 
>          -> Integer    -- ^ Start line (0-based)
>          -> Int        -- ^ Number of lines
>          -> IO [T.Text]
> getLines _lc _startLine _count = 
>   error "getLines is deprecated. Use getLinesFromStart/getLinesFromEnd/getLinesFrom instead."

> -- | DEPRECATED: Get a single line (convenience)
> -- Use getLinesFromStart, getLinesFromEnd, or getLinesFrom instead
> getLine :: LineCache -> Integer -> IO (Maybe T.Text)
> getLine _lc _lineNum = 
>   error "getLine is deprecated. Use getLinesFromStart/getLinesFromEnd/getLinesFrom instead."

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
> -- Always generates negative numbers: [-count, -count+1, ..., -2, -1]
> calculateBackwardLineNumbers :: Int -> [Integer]
> calculateBackwardLineNumbers count = 
>   if count <= 0 
>     then []
>     else [negate (fromIntegral count) .. (-1)]

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

> -- | Get total number of lines (lazy - scans if unknown)
> getTotalLines :: LineCache -> IO Integer
> getTotalLines lc = do
>   cached <- readIORef (lcTotalLines lc)
>   case cached of
>     Just total -> return total
>     Nothing -> do
>       -- TODO: Scan to find total
>       return 0  -- Placeholder

New Line-Oriented API with Positions
-------------------------------------

These functions provide a bidirectional line scanning API that returns line numbers
along with content, and an opaque position marker for resuming reads.

> -- | Read N lines from start of file (forward)
> -- Returns lines with positive line numbers [1, 2, 3, ...] and TWO positions
> -- topPosition: for scrolling up (backward), bottomPosition: for scrolling down (forward)
> getLinesFromStart :: LineCache -> Int 
>                   -> IO ([(T.Text, Integer)], LinePosition, LinePosition)
> getLinesFromStart lc count = do
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
>   -- Cache lines by offset
>   forM_ linesWithOffsets $ \(text, offset) ->
>     insertWithEviction lc offset text
>   
>   -- Generate line numbers starting from 1
>   let lineNumbers = calculateForwardLineNumbers 1 count
>       result = zip (map fst linesWithOffsets) lineNumbers
>   
>   -- Update sparse index with line number → offset mappings
>   let indexStep = lcIndexStep lc
>       indexEntries = [(lineNum, offset) | ((text, offset), lineNum) <- zip linesWithOffsets lineNumbers,
>                                            lineNum `mod` fromIntegral indexStep == 0]
>   sparseIdx <- readIORef (lcSparseIdx lc)
>   let sparseIdx' = SI.insertBatch indexEntries sparseIdx
>   writeIORef (lcSparseIdx lc) sparseIdx'
>   
>   -- Calculate TWO positions
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
>   return (result, topPos, bottomPos)

> -- | Read N lines from end of file (backward)
> -- Returns lines with negative line numbers [-N, -N+1, ..., -1] and TWO positions
> getLinesFromEnd :: LineCache -> Int 
>                 -> IO ([(T.Text, Integer)], LinePosition, LinePosition)
> getLinesFromEnd lc count = do
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
>   -- Cache lines by offset
>   forM_ linesWithOffsets $ \(text, offset) ->
>     insertWithEviction lc offset text
>   
>   -- Generate negative line numbers [-count, -count+1, ..., -1]
>   let lineNumbers = calculateBackwardLineNumbers count
>       result = zip (map fst linesWithOffsets) lineNumbers
>   
>   -- Update sparse index with line number → offset mappings
>   let indexStep = lcIndexStep lc
>       indexEntries = [(lineNum, offset) | ((text, offset), lineNum) <- zip linesWithOffsets lineNumbers,
>                                            lineNum `mod` fromIntegral indexStep == 0]
>   sparseIdx <- readIORef (lcSparseIdx lc)
>   let sparseIdx' = SI.insertBatch indexEntries sparseIdx
>   writeIORef (lcSparseIdx lc) sparseIdx'
>   
>   -- Calculate TWO positions
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
>   return (result, topPos, bottomPos)

> -- | Read N lines from a given position in specified direction
> -- The startLineNum parameter tells the cache what line number corresponds to the start position
> -- Returns lines with appropriate line numbers and TWO positions to continue
> getLinesFrom :: LineCache -> LinePosition -> Direction -> Int -> Integer
>              -> IO ([(T.Text, Integer)], LinePosition, LinePosition)
> getLinesFrom lc (LinePosition startOffset origin) dir count startLineNum = do
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
>   -- Adjust offsets to be absolute and cache lines by offset
>   let adjustedLines = case dir of
>         Forward  -> [(text, startOffset + off) | (text, off) <- linesWithOffsets]
>         Backward -> linesWithOffsets  -- Already absolute
>   
>   forM_ adjustedLines $ \(text, offset) ->
>     insertWithEviction lc offset text
>   
>   -- Calculate line numbers for the returned lines
>   -- The caller tells us the starting line number via startLineNum parameter
>   -- For backward scans, texts are in file order, so line numbers must be too
>   let lineNumbers = case dir of
>         Forward  -> [startLineNum .. startLineNum + fromIntegral count - 1]
>         Backward -> [startLineNum - fromIntegral count + 1 .. startLineNum]
>       texts = map fst adjustedLines
>       result = zip texts lineNumbers
>   
>   -- Update sparse index
>   let indexStep = lcIndexStep lc
>       indexEntries = [(lineNum, offset) | ((text, offset), lineNum) <- zip adjustedLines lineNumbers,
>                                            lineNum `mod` fromIntegral indexStep == 0]
>   sparseIdx <- readIORef (lcSparseIdx lc)
>   let sparseIdx' = SI.insertBatch indexEntries sparseIdx
>   writeIORef (lcSparseIdx lc) sparseIdx'
>   
>   -- Calculate TWO positions
>   let topOffset = case dir of
>         Forward  -> startOffset  -- Top stays when scrolling down
>         Backward -> if null adjustedLines then startOffset else snd (head adjustedLines)
>       bottomOffset = case dir of
>         Forward  -> if null adjustedLines 
>                     then startOffset
>                     else let (lastText, lastOff) = last adjustedLines
>                              textLen = fromIntegral (BS.length $ TE.encodeUtf8 lastText)
>                              lineEndLen = fromIntegral (lcLineEndingLen lc)
>                          -- text + line_ending (CR-LF=2, LF=1)
>                          in lastOff + textLen + lineEndLen
>         Backward -> startOffset  -- Bottom stays when scrolling up
>       topPos = LinePosition topOffset origin
>       bottomPos = LinePosition bottomOffset origin
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

