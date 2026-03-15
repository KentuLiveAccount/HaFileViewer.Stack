{-# LANGUAGE OverloadedStrings #-}
-- Test to reproduce page up/down bug
module Main where

import HaFileViewer.Backend.LineCache
import HaFileViewer.CUILogViewer.ViewState
import qualified HaFileViewer.CUILogViewer.Operations as Ops
import System.IO (writeFile)
import System.Directory (removeFile)
import qualified Data.Text as T

testFile :: FilePath
testFile = "test_page_bug.txt"

main :: IO ()
main = do
  -- Create a test file with 100 lines
  writeFile testFile $ unlines [show i | i <- [1..100]]
  
  -- Initialize viewer
  vs0 <- Ops.initializeViewer testFile 25
  
  putStrLn "=== Initial state ==="
  printViewport vs0
  
  -- Page down twice
  vs1 <- Ops.pageDown vs0
  putStrLn "\n=== After 1st page down ==="
  printViewport vs1
  
  vs2 <- Ops.pageDown vs1
  putStrLn "\n=== After 2nd page down ==="
  printViewport vs2
  
  -- Page up twice
  vs3 <- Ops.pageUp vs2
  putStrLn "\n=== After 1st page up ==="
  printViewport vs3
  
  vs4 <- Ops.pageUp vs3
  putStrLn "\n=== After 2nd page up (BUG: should show 1-25) ==="
  printViewport vs4
  
  -- Cleanup
  closeLineCache (vsCache vs0)
  removeFile testFile

printViewport :: ViewState -> IO ()
printViewport vs = do
  let viewport = vsViewport vs
      cursor = vsCursor vs
  if null viewport
    then putStrLn "Empty viewport"
    else do
      let (firstNum, firstText) = head viewport
          (lastNum, lastText) = last viewport
      putStrLn $ "First line: " ++ show firstNum ++ " \"" ++ T.unpack firstText ++ "\""
      putStrLn $ "Last line: " ++ show lastNum ++ " \"" ++ T.unpack lastText ++ "\""
      putStrLn $ "Cursor: firstLine=" ++ show (cursorFirstLine cursor) 
                 ++ " lastLine=" ++ show (cursorLastLine cursor)
