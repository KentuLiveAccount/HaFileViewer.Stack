LineCache: Primary Component for Efficient Line Access
=======================================================

This module provides the main line caching component for efficient
access to large files. It internally maintains both a sparse index
(for seeking) and a content cache (for repeated access).

> module HaFileViewer.LineCache
>   ( -- * Types
>     LineCache
>   , CacheConfig(..)
>   , CacheStats(..)
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
>     -- * Cache management
>   , clearCache
>   , invalidateCache
>   , getCacheStats
>     
>     -- * Configuration
>   , defaultConfig
>   ) where

> import Prelude hiding (lookup, getLine)
> import qualified Data.Map.Strict as Map
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as TE
> import qualified Data.ByteString as BS
> import Data.IORef
> import Data.Time (UTCTime, getCurrentTime)
> import System.IO hiding (getLine)
> import System.Directory (getModificationTime, getFileSize)
> import Control.Exception (bracket)
> import Control.Monad (when)
> 
> import HaFileViewer.BidirectionalScanner 
>   ( scanLines, Direction(..) )
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
>       -- TODO: Implement scanning with cache population
>       return []  -- Placeholder

> -- | Get a single line (convenience)
> getLine :: LineCache -> Integer -> IO (Maybe T.Text)
> getLine lc lineNum = do
>   lines <- getLines lc lineNum 1
>   return $ case lines of
>     (l:_) -> Just l
>     []    -> Nothing

> -- | Get total number of lines (lazy - scans if unknown)
> getTotalLines :: LineCache -> IO Integer
> getTotalLines lc = do
>   cached <- readIORef (lcTotalLines lc)
>   case cached of
>     Just total -> return total
>     Nothing -> do
>       -- TODO: Scan to find total
>       return 0  -- Placeholder

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
