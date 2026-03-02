{-# LANGUAGE OverloadedStrings #-}

-- Test file for Step 5: cursor update logic functions
-- 8 unit tests covering forward and backward cursor updates

module Main where

import qualified Data.Text as T
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
  putStrLn "Testing cursor update logic functions..."
  putStrLn ""
  
  -- Test 1: Forward: cursor(0,0,FromStart) + 10 lines ending at offset 1000 -> cursor(1000,10,FromStart)
  let cursor1 = ViewCursor { cursorOffset = 0, cursorLineNum = 0, cursorOrigin = FromStart }
  let lines1 = [(T.pack ("line" ++ show n), fromIntegral (n * 100)) | n <- [1..10]]
  let result1 = updateCursorForward cursor1 lines1
  runTest "Test 1: Forward cursor(0,0,FromStart) + 10 lines -> cursor(1000,10,FromStart)"
    (cursorOffset result1 == 1000 && cursorLineNum result1 == 10 && cursorOrigin result1 == FromStart)
  
  -- Test 2: Forward: cursor(100,50,FromStart) + 25 lines ending at offset 2000 -> cursor(2000,75,FromStart)
  let cursor2 = ViewCursor { cursorOffset = 100, cursorLineNum = 50, cursorOrigin = FromStart }
  let lines2 = [(T.pack ("line" ++ show n), fromIntegral (n * 40 + 1000)) | n <- [1..25]]
  let result2 = updateCursorForward cursor2 lines2
  runTest "Test 2: Forward cursor(100,50,FromStart) + 25 lines -> cursor(2000,75,FromStart)"
    (cursorOffset result2 == 2000 && cursorLineNum result2 == 75 && cursorOrigin result2 == FromStart)
  
  -- Test 3: Backward: cursor(10000,0,FromEnd) + 10 lines starting at offset 9000 -> cursor(9000,10,FromEnd)
  let cursor3 = ViewCursor { cursorOffset = 10000, cursorLineNum = 0, cursorOrigin = FromEnd }
  let lines3 = [(T.pack ("line" ++ show n), fromIntegral (9000 + n * 10)) | n <- [0..9]]
  let result3 = updateCursorBackward cursor3 lines3
  runTest "Test 3: Backward cursor(10000,0,FromEnd) + 10 lines -> cursor(9000,10,FromEnd)"
    (cursorOffset result3 == 9000 && cursorLineNum result3 == 10 && cursorOrigin result3 == FromEnd)
  
  -- Test 4: Backward: cursor(5000,50,FromEnd) + 25 lines starting at offset 3000 -> cursor(3000,75,FromEnd)
  let cursor4 = ViewCursor { cursorOffset = 5000, cursorLineNum = 50, cursorOrigin = FromEnd }
  let lines4 = [(T.pack ("line" ++ show n), fromIntegral (3000 + n * 40)) | n <- [0..24]]
  let result4 = updateCursorBackward cursor4 lines4
  runTest "Test 4: Backward cursor(5000,50,FromEnd) + 25 lines -> cursor(3000,75,FromEnd)"
    (cursorOffset result4 == 3000 && cursorLineNum result4 == 75 && cursorOrigin result4 == FromEnd)
  
  -- Test 5: Forward with empty result (EOF) -> cursor unchanged
  let cursor5 = ViewCursor { cursorOffset = 5000, cursorLineNum = 100, cursorOrigin = FromStart }
  let lines5 = []
  let result5 = updateCursorForward cursor5 lines5
  runTest "Test 5: Forward with empty result (EOF) -> cursor unchanged"
    (cursorOffset result5 == 5000 && cursorLineNum result5 == 100 && cursorOrigin result5 == FromStart)
  
  -- Test 6: Backward with empty result (BOF) -> cursor unchanged
  let cursor6 = ViewCursor { cursorOffset = 1000, cursorLineNum = 50, cursorOrigin = FromEnd }
  let lines6 = []
  let result6 = updateCursorBackward cursor6 lines6
  runTest "Test 6: Backward with empty result (BOF) -> cursor unchanged"
    (cursorOffset result6 == 1000 && cursorLineNum result6 == 50 && cursorOrigin result6 == FromEnd)
  
  -- Test 7: Verify offset taken from last line (forward)
  let cursor7 = ViewCursor { cursorOffset = 0, cursorLineNum = 0, cursorOrigin = FromStart }
  let lines7 = [("line1", 100), ("line2", 250), ("line3", 500)]
  let result7 = updateCursorForward cursor7 lines7
  runTest "Test 7: Forward offset taken from last line"
    (cursorOffset result7 == 500 && cursorLineNum result7 == 3)
  
  -- Test 8: Verify offset taken from first line (backward)
  let cursor8 = ViewCursor { cursorOffset = 10000, cursorLineNum = 0, cursorOrigin = FromEnd }
  let lines8 = [("line1", 7500), ("line2", 8250), ("line3", 9000)]
  let result8 = updateCursorBackward cursor8 lines8
  runTest "Test 8: Backward offset taken from first line"
    (cursorOffset result8 == 7500 && cursorLineNum result8 == 3)
  
  putStrLn ""
  putStrLn "All 8 tests passed!"
  exitSuccess
