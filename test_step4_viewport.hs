{-# LANGUAGE OverloadedStrings #-}

-- Test file for Step 4: viewport shifting functions
-- 6 unit tests covering viewport shifting operations

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
  putStrLn "Testing viewport shifting functions..."
  putStrLn ""
  
  -- Test 1: shiftDown: viewport [(1,"a"), (2,"b")...(25,"y")] + (26,"z") size 25 -> [(2,"b")...(26,"z")]
  let viewport1 = [(fromIntegral n, T.pack [c]) | (n, c) <- zip [1..25] ['a'..'y']]
  let newLine1 = (26, "z")
  let result1 = shiftViewportDown viewport1 newLine1 25
  let expected1 = [(fromIntegral n, T.pack [c]) | (n, c) <- zip [2..26] ['b'..'z']]
  runTest "Test 1: shiftDown viewport [1..25] + 26 -> [2..26]"
    (result1 == expected1 && length result1 == 25)
  
  -- Test 2: shiftDown: viewport [(10,"j")...(34,"h")] + (35,"i") size 25 -> [(11,"k")...(35,"i")]
  let viewport2 = [(fromIntegral n, T.pack [c]) | (n, c) <- zip [10..34] (['j'..'z'] ++ ['a'..'h'])]
  let newLine2 = (35, "i")
  let result2 = shiftViewportDown viewport2 newLine2 25
  let expected2 = [(fromIntegral n, T.pack [c]) | (n, c) <- zip [11..35] (['k'..'z'] ++ ['a'..'i'])]
  runTest "Test 2: shiftDown viewport [10..34] + 35 -> [11..35]"
    (result2 == expected2 && length result2 == 25)
  
  -- Test 3: shiftUp: (0,"zero") + viewport [(1,"a")...(25,"y")] size 25 -> [(0,"zero")...(24,"x")]
  let viewport3 = [(fromIntegral n, T.pack [c]) | (n, c) <- zip [1..25] ['a'..'y']]
  let newLine3 = (0, "zero")
  let result3 = shiftViewportUp newLine3 viewport3 25
  let expected3 = (0, "zero") : [(fromIntegral n, T.pack [c]) | (n, c) <- zip [1..24] ['a'..'x']]
  runTest "Test 3: shiftUp (0,zero) + viewport [1..25] -> [0..24]"
    (result3 == expected3 && length result3 == 25)
  
  -- Test 4: Edge: shift down with 1-line viewport -> works correctly
  let viewport4 = [(1, "first")]
  let newLine4 = (2, "second")
  let result4 = shiftViewportDown viewport4 newLine4 1
  let expected4 = [(2, "second")]
  runTest "Test 4: shiftDown with 1-line viewport"
    (result4 == expected4 && length result4 == 1)
  
  -- Test 5: Edge: shift with empty viewport -> returns single line
  let viewport5 = []
  let newLine5 = (1, "first")
  let result5 = shiftViewportDown viewport5 newLine5 1
  let expected5 = [(1, "first")]
  runTest "Test 5: shiftDown with empty viewport"
    (result5 == expected5 && length result5 == 1)
  
  -- Test 6: Size: verify size maintained after multiple shifts
  let viewport6 = [(fromIntegral n, T.pack $ show n) | n <- [1..25]]
  let shift1 = shiftViewportDown viewport6 (26, "26") 25
  let shift2 = shiftViewportDown shift1 (27, "27") 25
  let shift3 = shiftViewportDown shift2 (28, "28") 25
  runTest "Test 6: size maintained after multiple shifts"
    (length shift1 == 25 && length shift2 == 25 && length shift3 == 25 &&
     fst (head shift3) == 4 && fst (last shift3) == 28)
  
  putStrLn ""
  putStrLn "All 6 tests passed!"
  exitSuccess
