{-# LANGUAGE OverloadedStrings #-}
module Main where

import HaFileViewer.LineMap
import qualified Data.Text as T
import System.Environment (getArgs)
import System.IO
import Data.IORef
import Text.Read (readMaybe)

defaultPageLines :: Int
defaultPageLines = 25

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> run path defaultPageLines
    [path, pStr] | Just p <- readMaybe pStr -> run path p
    _ -> putStrLn "Usage: ha-file-viewer-cui <file> [pageLines]"

run :: FilePath -> Int -> IO ()
run path pageLines = do
  lm <- openLineMap path indexStepDefault
  hSetBuffering stdin NoBuffering
  putStrLn $ "Opened file: " ++ path ++ " (page lines = " ++ show pageLines ++ ")"
  posRef <- newIORef 0
  loop lm posRef pageLines
  closeLineMap lm

loop :: LineMap -> IORef Integer -> Int -> IO ()
loop lm posRef pageLines = do
  pos <- readIORef posRef
  ls <- getLines lm pos pageLines
  putStrLn $ "-- lines " ++ show pos ++ " to " ++ show (pos + fromIntegral (length ls) - 1) ++ " --"
  mapM_ (putStrLn . T.unpack) ls
  putStrLn "Commands: n (next), p (prev), g (goto), q (quit)"
  putStr "> "
  hFlush stdout
  cmdLine <- getLine
  case cmdLine of
    "n" -> do
      let newPos = pos + fromIntegral pageLines
      writeIORef posRef newPos
      loop lm posRef pageLines
    "p" -> do
      let newPos = max 0 (pos - fromIntegral pageLines)
      writeIORef posRef newPos
      loop lm posRef pageLines
    ('g':rest) -> case readMaybe (dropWhile (==' ') rest) of
      Just n | n >= 0 -> do writeIORef posRef n; loop lm posRef pageLines
      _ -> putStrLn "Usage: g <lineNumber>" >> loop lm posRef pageLines
    "q" -> return ()
    _ -> putStrLn "Unknown command" >> loop lm posRef pageLines
