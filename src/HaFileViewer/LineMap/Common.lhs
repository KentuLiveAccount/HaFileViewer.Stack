Common Utilities for LineMap Implementations
============================================

This module contains shared utilities used by both LineMap and LineMap2
implementations, including text processing, memory-mapped file access,
and common constants.

Module Header
-------------

> {-# LANGUAGE OverloadedStrings #-}
> module HaFileViewer.LineMap.Common
>   ( Offset
>   , lfByte
>   , normalizeLine
>   , decodeUtf8Lenient
>   , ensureMapped
>   , readAtOffset
>   ) where
>
> import qualified Data.ByteString as BS
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as TE
> import qualified Data.Text.Encoding.Error as TEE
> import System.IO.MMap (mmapFileByteString)
> import Data.Word
> import Data.IORef
> import Control.Monad (when)

Type Aliases
------------

> type Offset = Integer

Constants
---------

Line feed byte (newline character).

> lfByte :: Word8
> lfByte = 10

Text Processing
---------------

Remove Windows-style carriage return from end of line.

> normalizeLine :: T.Text -> T.Text
> normalizeLine = T.dropWhileEnd (== '\r')

Decode UTF-8 with lenient error handling (replaces invalid sequences).

> decodeUtf8Lenient :: BS.ByteString -> T.Text
> decodeUtf8Lenient = TE.decodeUtf8With TEE.lenientDecode

Memory-Mapped File Access
--------------------------

Ensure that a memory window covers the requested offset and size.
Remaps the window if necessary, centering it around the requested offset.

Parameters:
- filePath: Path to the file being mapped
- fileSize: Total size of the file in bytes
- windowSize: Size of the memory-mapped window
- windowRef: IORef holding current (offset, data) window
- offset: Requested offset to access
- size: Number of bytes needed at that offset

> ensureMapped :: FilePath -> Integer -> Integer -> IORef (Offset, BS.ByteString)
>              -> Offset -> Integer -> IO ()
> ensureMapped filePath fileSize windowSize windowRef offset size = do
>   (winStart, winData) <- readIORef windowRef
>   let winEnd = winStart + fromIntegral (BS.length winData)
>       needsRemap = offset < winStart || offset + size > winEnd
>   
>   when needsRemap $ do
>     -- Center the new window around the requested offset
>     let halfWin = windowSize `div` 2
>         newStart = max 0 (offset - halfWin)
>         maxSize = fileSize - newStart
>         newSize = min windowSize maxSize
>         mapSpec = if newSize > 0 
>                   then Just (fromIntegral newStart, fromIntegral newSize)
>                   else Nothing
>     newData <- if newSize > 0
>                then mmapFileByteString filePath mapSpec
>                else return BS.empty
>     writeIORef windowRef (newStart, newData)

Read bytes from file at an absolute offset, automatically handling window mapping.

Parameters:
- filePath: Path to the file being mapped
- fileSize: Total size of the file in bytes  
- windowSize: Size of the memory-mapped window
- windowRef: IORef holding current (offset, data) window
- offset: Absolute file offset to read from
- size: Number of bytes to read

> readAtOffset :: FilePath -> Integer -> Integer -> IORef (Offset, BS.ByteString)
>              -> Offset -> Integer -> IO BS.ByteString
> readAtOffset filePath fileSize windowSize windowRef offset size = do
>   ensureMapped filePath fileSize windowSize windowRef offset size
>   (winStart, winData) <- readIORef windowRef
>   let relOffset = offset - winStart
>       chunk = BS.take (fromIntegral size) $ BS.drop (fromIntegral relOffset) winData
>   return chunk
