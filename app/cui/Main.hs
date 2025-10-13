{-# LANGUAGE OverloadedStrings #-}
module Main where

import HaFileViewer.Backend
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import System.Environment (getArgs)
import System.IO
import Data.Int (Int64)

pageSizeBytes :: Int
pageSizeBytes = 4096

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> run path
    _ -> putStrLn "Usage: ha-file-viewer-cui <file>"

run :: FilePath -> IO ()
run path = do
  fh <- openFileHandle path
  putStrLn $ "Opened file, size = " ++ show (fileSize fh) ++ " bytes"
  loop fh 0
  closeFileHandle fh

loop :: FileHandle -> Integer -> IO ()
loop fh off = do
  chunk <- readChunkAt fh off pageSizeBytes
  C8.putStrLn chunk
  putStrLn "n: next, p: prev, q: quit"
  hFlush stdout
  c <- getChar
  _ <- getLine -- consume rest of line
  case c of
    'n' -> loop fh (min (fileSize fh) (off + fromIntegral pageSizeBytes))
    'p' -> loop fh (max 0 (off - fromIntegral pageSizeBytes))
    'q' -> return ()
    _   -> loop fh off
