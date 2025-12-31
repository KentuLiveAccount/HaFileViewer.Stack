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
>   }

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
>   return $ LineMap path fileSize windowSizeDefault winRef k indexRef totalRef

> closeLineMap :: LineMap -> IO ()
> closeLineMap _lm = return ()

Main API function (stub for now).

> getLines :: LineMap -> Integer -> Int -> IO [T.Text]
> getLines _lm _start _count = do
>   -- TODO: Implement using new structure
>   return []

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
