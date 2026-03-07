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

> module HaFileViewer.LineCache
>   ( -- * Types
>     LineCache
>   , CacheConfig(..)
>   , CacheStats(..)
>   , LinePosition  -- Opaque type, constructor not exported
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
> import Control.Monad (when)
> 
> import HaFileViewer.BidirectionalScanner 
>   ( scanLines, scanLinesWithOffsets, Direction(..) )
> import HaFileViewer.LineMap.Common (Offset)
> import qualified HaFileViewer.Internal.SparseIndex as SI

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
>     lcFilePath    :: FilePath
>   , lcFileSize    :: Integer
>   , lcFileModTime :: IORef UTCTime
>   , lcHandle      :: IORef (Maybe Handle)
>     
>     -- Sparse index (internal optimization)
>   , lcSparseIdx   :: IORef SI.SparseIndex
>   , lcIndexStep   :: Int
>     
>     -- Content cache (primary storage)
>   , lcContent     :: IORef (Map.Map Integer T.Text)
>   , lcLRUOrder    :: IORef [Integer]  -- Most recent last
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

> -- | Opaque position marker for resuming line reads
> -- Wraps a byte offset but keeps it hidden from API consumers
> newtype LinePosition = LinePosition Offset
>   deriving (Show, Eq)

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
> getLines :: LineCache 
>          -> Integer    -- ^ Start line (0-based)
>          -> Int        -- ^ Number of lines
>          -> IO [T.Text]
> getLines lc startLine count = do
>   -- Check if file modified
>   modified <- checkModified lc
>   when modified $ invalidateCache lc
>   
>   -- Try content cache first
>   cached <- tryContentCache lc startLine count
>   case cached of
>     Just lines -> return lines
>     Nothing -> do
>       -- Cache miss - need to scan
>       -- Find best starting point using sparse index
>       (scanStartLine, scanStartOffset) <- findStartOffset lc startLine
>       
>       -- Debug output
>       -- putStrLn $ "DEBUG: Scanning from line " ++ show scanStartLine ++ " offset " ++ show scanStartOffset
>       
>       -- Scan from that offset to get all lines up to and including our target
>       scannedLines <- scanFromOffset lc scanStartOffset scanStartLine startLine count
>       
>       -- Cache everything we scanned (bulk insert with LRU)
>       cacheResult lc scannedLines
>       
>       -- Extract and return just the requested range
>       let result = extractRange startLine count scannedLines
>       -- putStrLn $ "DEBUG: Extracted " ++ show (length result) ++ " lines from range"
>       return result

> -- | Get a single line (convenience)
> getLine :: LineCache -> Integer -> IO (Maybe T.Text)
> getLine lc lineNum = do
>   lines <- getLines lc lineNum 1
>   return $ case lines of
>     (l:_) -> Just l
>     []    -> Nothing

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
> -- Returns lines with positive line numbers [1, 2, 3, ...] and position to continue
> getLinesFromStart :: LineCache -> Int -> IO ([(T.Text, Integer)], LinePosition)
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
>   -- Generate line numbers starting from 1 (not 0-based)
>   let lineNumbers = calculateForwardLineNumbers 1 count
>       result = zip (map fst linesWithOffsets) lineNumbers
>   
>   -- Calculate new position (offset after last line read)
>   let newPosition = LinePosition $ extractNewPosition linesWithOffsets Forward
>   
>   return (result, newPosition)

> -- | Read N lines from end of file (backward)
> -- Returns lines with negative line numbers [-N, -N+1, ..., -1] and position to continue
> getLinesFromEnd :: LineCache -> Int -> IO ([(T.Text, Integer)], LinePosition)
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
>   -- Generate negative line numbers [-count, -count+1, ..., -1]
>   let lineNumbers = calculateBackwardLineNumbers count
>       result = zip (map fst linesWithOffsets) lineNumbers
>   
>   -- Calculate new position (offset of first line for backward)
>   let newPosition = LinePosition $ extractNewPosition linesWithOffsets Backward
>   
>   return (result, newPosition)

> -- | Read N lines from a given position in specified direction
> -- Returns lines with appropriate line numbers and new position to continue
> getLinesFrom :: LineCache -> LinePosition -> Direction -> Int 
>              -> IO ([(T.Text, Integer)], LinePosition)
> getLinesFrom lc (LinePosition startOffset) dir count = do
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
>   -- Adjust offsets to be absolute (scanLinesWithOffsets returns relative offsets for Forward)
>   let adjustedLines = case dir of
>         Forward  -> [(text, startOffset + off) | (text, off) <- linesWithOffsets]
>         Backward -> linesWithOffsets  -- Backward offsets are already absolute
>   
>   -- Generate line numbers based on direction
>   -- Note: We use 0-based here since we don't know the absolute position in file
>   let lineNumbers = case dir of
>         Forward  -> calculateForwardLineNumbers 0 count
>         Backward -> calculateBackwardLineNumbers count
>       result = zip (map fst adjustedLines) lineNumbers
>   
>   -- Calculate new position
>   let newPosition = LinePosition $ extractNewPosition adjustedLines dir
>   
>   return (result, newPosition)

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

> -- | Try to get lines from content cache
> -- Returns Nothing if any line is missing
> tryContentCache :: LineCache -> Integer -> Int -> IO (Maybe [T.Text])
> tryContentCache lc startLine count = do
>   cache <- readIORef (lcContent lc)
>   let lineNums = [startLine .. startLine + fromIntegral count - 1]
>   let cachedLines = mapM (\ln -> Map.lookup ln cache) lineNums
>   case cachedLines of
>     Just lines -> do
>       -- Update LRU order for cache hits
>       updateLRU lc lineNums
>       return $ Just lines
>     Nothing -> return Nothing

> -- | Update LRU order (move accessed lines to end)
> updateLRU :: LineCache -> [Integer] -> IO ()
> updateLRU lc accessed = do
>   lru <- readIORef (lcLRUOrder lc)
>   let lru' = filter (`notElem` accessed) lru ++ accessed
>   writeIORef (lcLRUOrder lc) lru'

> -- | Find the best starting offset for scanning using sparse index
> -- Returns (startLineNum, startOffset)
> findStartOffset :: LineCache -> Integer -> IO (Integer, Offset)
> findStartOffset lc targetLine = do
>   sparseIdx <- readIORef (lcSparseIdx lc)
>   case SI.lookupNearest targetLine sparseIdx of
>     Just (lineNum, offset) -> return (lineNum, offset)
>     Nothing -> return (0, 0)  -- Start from beginning if no index

> -- | Scan from a given offset and collect lines with their byte offsets
> -- Returns list of (lineNum, lineContent, byteOffset) for all lines scanned
> scanFromOffset :: LineCache 
>                -> Offset      -- ^ Starting offset
>                -> Integer     -- ^ Starting line number
>                -> Integer     -- ^ Target line number
>                -> Int         -- ^ Number of lines to get
>                -> IO [(Integer, T.Text, Offset)]
> scanFromOffset lc startOffset startLine targetLine count = do
>   -- Open file handle if not already open
>   h <- ensureHandle lc
>   
>   -- Calculate how many lines we need to scan
>   let linesToScan = fromInteger (targetLine - startLine) + count
>       fileSize = lcFileSize lc
>   
>   -- Create read function for scanLines
>   let readFn offset size = do
>         hSeek h AbsoluteSeek (fromInteger offset)
>         BS.hGet h (fromInteger size)
>   
>   -- Scan forward from startOffset using new API with offsets
>   let adjustedReadFn off size = readFn (startOffset + off) size
>       remainingSize = fileSize - startOffset
>   
>   linesWithOffsets <- scanLinesWithOffsets Forward remainingSize adjustedReadFn linesToScan
>   
>   -- Pair with line numbers and adjust offsets (scanLinesWithOffsets returns offsets relative to read start)
>   let lineNums = [startLine..]
>       result = [(lineNum, text, startOffset + off) 
>                | (lineNum, (text, off)) <- zip lineNums linesWithOffsets]
>   
>   return result

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

> -- | Cache the scan result with LRU eviction and sparse index updates
> cacheResult :: LineCache -> [(Integer, T.Text, Offset)] -> IO ()
> cacheResult lc scannedLines = do
>   -- Update sparse index for every Kth line with REAL offsets
>   let indexStep = lcIndexStep lc
>       indexEntries = [(ln, off) | (ln, _, off) <- scannedLines, ln `mod` fromIntegral indexStep == 0]
>   
>   sparseIdx <- readIORef (lcSparseIdx lc)
>   let sparseIdx' = SI.insertBatch indexEntries sparseIdx
>   writeIORef (lcSparseIdx lc) sparseIdx'
>   
>   -- Insert lines into content cache with LRU eviction
>   mapM_ (\(ln, text, _off) -> insertWithEviction lc ln text) scannedLines

> -- | Insert a single line into cache with LRU eviction
> insertWithEviction :: LineCache -> Integer -> T.Text -> IO ()
> insertWithEviction lc lineNum content = do
>   cache <- readIORef (lcContent lc)
>   lru <- readIORef (lcLRUOrder lc)
>   
>   -- Check if we need to evict
>   let currentSize = Map.size cache
>       maxSize = lcMaxContent lc
>   
>   (cache', lru') <- if currentSize >= maxSize && lineNum `Map.notMember` cache
>     then do
>       -- Need to evict oldest entry
>       case lru of
>         (oldest:rest) -> do
>           let cache'' = Map.delete oldest cache
>           return (cache'', rest)
>         [] -> return (cache, lru)
>     else return (cache, lru)
>   
>   -- Insert new line
>   let cache'' = Map.insert lineNum content cache'
>       lru'' = filter (/= lineNum) lru' ++ [lineNum]
>   
>   writeIORef (lcContent lc) cache''
>   writeIORef (lcLRUOrder lc) lru''

> -- | Extract requested range from scanned lines
> extractRange :: Integer -> Int -> [(Integer, T.Text, Offset)] -> [T.Text]
> extractRange startLine count scannedLines =
>   let endLine = startLine + fromInteger (toInteger count) - 1
>       inRange (ln, _, _) = ln >= startLine && ln <= endLine
>       rangeLines = filter inRange scannedLines
>   in map (\(_, text, _) -> text) rangeLines
