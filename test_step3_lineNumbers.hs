{-# LANGUAGE OverloadedStrings #-}

-- Test file for Step 3: calculateDisplayLineNumber function
-- 8 unit tests covering positive and negative line numbering

module Main where

import HaFileViewer.CUILogViewer.ViewState
import System.Exit (exitFailure, exitSuccess)

-- Test helper
runTest :: String -> Bool -> IO ()
runTest name result = do
  if result
    then putStrLn $ "[PASS] " ++ name
    else do
      putStrLn $ "[FAIL] " ++ name
      exitFailure

main :: IO ()
main = do
  putStrLn "Testing calculateDisplayLineNumber..."
  putStrLn ""
  
  -- Test 1: FromStart, lineNum=0, index 0 -> 1 (first line)
  let cursor1 = ViewCursor { cursorOffset = 0, cursorLineNum = 0, cursorOrigin = FromStart }
  runTest "Test 1: FromStart lineNum=0 index=0 -> 1" 
    (calculateDisplayLineNumber cursor1 0 == 1)
  
  -- Test 2: FromStart, lineNum=0, index 5 -> 6
  runTest "Test 2: FromStart lineNum=0 index=5 -> 6"
    (calculateDisplayLineNumber cursor1 5 == 6)
  
  -- Test 3: FromStart, lineNum=100, index 0 -> 101 (scrolled forward)
  let cursor3 = ViewCursor { cursorOffset = 5000, cursorLineNum = 100, cursorOrigin = FromStart }
  runTest "Test 3: FromStart lineNum=100 index=0 -> 101"
    (calculateDisplayLineNumber cursor3 0 == 101)
  
  -- Test 4: FromStart, lineNum=100, index 10 -> 111
  runTest "Test 4: FromStart lineNum=100 index=10 -> 111"
    (calculateDisplayLineNumber cursor3 10 == 111)
  
  -- Test 5: FromEnd, lineNum=0, index 0 -> -1 (last line)
  let cursor5 = ViewCursor { cursorOffset = 10000, cursorLineNum = 0, cursorOrigin = FromEnd }
  runTest "Test 5: FromEnd lineNum=0 index=0 -> -1"
    (calculateDisplayLineNumber cursor5 0 == -1)
  
  -- Test 6: FromEnd, lineNum=0, index 5 -> -6
  runTest "Test 6: FromEnd lineNum=0 index=5 -> -6"
    (calculateDisplayLineNumber cursor5 5 == -6)
  
  -- Test 7: FromEnd, lineNum=100, index 0 -> -101 (scrolled backward)
  let cursor7 = ViewCursor { cursorOffset = 2000, cursorLineNum = 100, cursorOrigin = FromEnd }
  runTest "Test 7: FromEnd lineNum=100 index=0 -> -101"
    (calculateDisplayLineNumber cursor7 0 == -101)
  
  -- Test 8: FromEnd, lineNum=100, index 10 -> -111
  runTest "Test 8: FromEnd lineNum=100 index=10 -> -111"
    (calculateDisplayLineNumber cursor7 10 == -111)
  
  putStrLn ""
  putStrLn "All 8 tests passed!"
  exitSuccess
