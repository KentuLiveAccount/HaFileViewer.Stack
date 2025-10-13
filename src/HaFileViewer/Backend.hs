{-# LANGUAGE OverloadedStrings #-}
module HaFileViewer.Backend
  ( FileHandle
  , openFileHandle
  , closeFileHandle
  , readChunkAt
  , fileSize
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.IO
import System.FilePath
import System.Directory
import Data.Int (Int64)  -- Keep this import for compatibility

-- | Opaque handle wrapping a Seekable Handle and cached size
data FileHandle = FileHandle
  { fhHandle :: Handle
  , fhSize   :: Integer
  }

-- | Open a file for binary random access. Caller should call 'closeFileHandle'.
openFileHandle :: FilePath -> IO FileHandle
openFileHandle path = do
  h <- openBinaryFile path ReadMode
  sz <- hFileSize h
  return $ FileHandle h sz

-- | Close the file handle
closeFileHandle :: FileHandle -> IO ()
closeFileHandle = hClose . fhHandle

-- | Read up to n bytes starting at the given absolute byte offset.
-- Returns a strict ByteString (small chunks) - suitable for rendering lines.
readChunkAt :: FileHandle -> Integer -> Int -> IO BS.ByteString
readChunkAt fh off len = do
  let h = fhHandle fh
  hSeek h AbsoluteSeek off
  BS.hGet h len

-- | Get file size in bytes
fileSize :: FileHandle -> Integer
fileSize = fhSize

-- Note: This is a minimal backend. For gigabyte-scale files consider
-- implementing memory-mapped IO (mmap) via the 'mmap' package or using
-- a chunked index of line offsets to provide fast seeking by line.
