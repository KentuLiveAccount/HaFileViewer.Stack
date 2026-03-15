{-# LANGUAGE OverloadedStrings #-}
-- Debug what getLinesFrom returns for backward scans
module Main where

import HaFileViewer.Backend.LineCache
import HaFileViewer.Backend.BidirectionalScanner (Direction(..))
import System.IO (writeFile)
import System.Directory (removeFile)
import qualified Data.Text as T

main :: IO ()
main = do
  writeFile "debug.txt" $ unlines ["Line " ++ show i | i <- [1..100]]
  
  cache <- openLineCache "debug.txt"
  
  -- Get first 25 lines
  (lines1, top1, bot1) <- getLinesFromStart cache 25
  putStrLn "=== getLinesFromStart (1-25) ==="
  putStrLn $ "API returns " ++ show (length lines1) ++ " lines:"
  mapM_ (\(text, lineNum) -> putStrLn $ "  (" ++ show lineNum ++ ", \"" ++ T.unpack text ++ "\")") (take 3 lines1)
  putStrLn "  ..."
  mapM_ (\(text, lineNum) -> putStrLn $ "  (" ++ show lineNum ++ ", \"" ++ T.unpack text ++ "\")") (drop 22 lines1)
  
  -- Page down once (get lines 26-50)
  (lines2, top2, bot2) <- getLinesFrom cache bot1 Forward 25 26
  putStrLn "\n=== getLinesFrom Forward (26-50) ==="
  mapM_ (\(text, lineNum) -> putStrLn $ "  (" ++ show lineNum ++ ", \"" ++ T.unpack text ++ "\")") (take 3 lines2)
  putStrLn "  ..."
  
  -- Page up (get lines 1-25 backwards from top2)
  (lines3, top3, bot3) <- getLinesFrom cache top2 Backward 25 25
  putStrLn "\n=== getLinesFrom Backward from line 26 ==="
  putStrLn $ "API returns " ++ show (length lines3) ++ " lines:"
  mapM_ (\(text, lineNum) -> putStrLn $ "  (" ++ show lineNum ++ ", \"" ++ T.unpack text ++ "\")") lines3
  
  closeLineCache cache
  removeFile "debug.txt"
