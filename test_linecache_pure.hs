{-# LANGUAGE OverloadedStrings #-}

-- Unit tests for pure helper functions in LineCache
-- These test calculateForwardLineNumbers, calculateBackwardLineNumbers, and extractNewPosition

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import HaFileViewer.LineCache
import HaFileViewer.BidirectionalScanner (Direction(..))

-- Test calculateForwardLineNumbers (6 tests)

test1 :: Bool
test1 = calculateForwardLineNumbers 1 3 == [1, 2, 3]

test2 :: Bool
test2 = calculateForwardLineNumbers 0 1 == [0]

test3 :: Bool
test3 = calculateForwardLineNumbers 100 5 == [100, 101, 102, 103, 104]

test4 :: Bool
test4 = calculateForwardLineNumbers 1 0 == []

test5 :: Bool
test5 = calculateForwardLineNumbers (-5) 3 == [-5, -4, -3]

test6 :: Bool
test6 = calculateForwardLineNumbers 999 2 == [999, 1000]

-- Test calculateBackwardLineNumbers (5 tests)

test7 :: Bool
test7 = calculateBackwardLineNumbers 3 == [-3, -2, -1]

test8 :: Bool
test8 = calculateBackwardLineNumbers 1 == [-1]

test9 :: Bool
test9 = calculateBackwardLineNumbers 5 == [-5, -4, -3, -2, -1]

test10 :: Bool
test10 = calculateBackwardLineNumbers 0 == []

test11 :: Bool
test11 = calculateBackwardLineNumbers 10 == [-10, -9, -8, -7, -6, -5, -4, -3, -2, -1]

-- Test extractNewPosition (4 tests)

-- Helper to create test data
mkTestLine :: String -> Int -> (T.Text, Integer)
mkTestLine str offset = (T.pack str, fromIntegral offset)

test12 :: Bool
test12 = extractNewPosition [mkTestLine "Hello" 100, mkTestLine "World" 200] Forward == 206
-- "World" is 5 bytes + 1 newline, so 200 + 5 + 1 = 206

test13 :: Bool
test13 = extractNewPosition [mkTestLine "Test" 100] Forward == 105
-- "Test" is 4 bytes + 1 newline, so 100 + 4 + 1 = 105

test14 :: Bool
test14 = extractNewPosition [mkTestLine "Hello" 100, mkTestLine "World" 200] Backward == 100
-- Backward takes first line's offset

test15 :: Bool
test15 = extractNewPosition [mkTestLine "Test" 100] Backward == 100
-- Single line backward also takes first offset

-- Run all tests

allTests :: [(String, Bool)]
allTests = 
  [ ("test1: forward from 1, count 3", test1)
  , ("test2: forward from 0, count 1", test2)
  , ("test3: forward from 100, count 5", test3)
  , ("test4: forward with count 0", test4)
  , ("test5: forward from negative", test5)
  , ("test6: forward from 999", test6)
  , ("test7: backward count 3", test7)
  , ("test8: backward count 1", test8)
  , ("test9: backward count 5", test9)
  , ("test10: backward count 0", test10)
  , ("test11: backward count 10", test11)
  , ("test12: extractNewPosition forward multiple lines", test12)
  , ("test13: extractNewPosition forward single line", test13)
  , ("test14: extractNewPosition backward multiple lines", test14)
  , ("test15: extractNewPosition backward single line", test15)
  ]

main :: IO ()
main = do
  putStrLn "Running LineCache pure function tests..."
  putStrLn ""
  
  let results = map (\(name, test) -> (name, test)) allTests
      passed = length $ filter snd results
      total = length results
  
  mapM_ printResult results
  
  putStrLn ""
  putStrLn $ "Results: " ++ show passed ++ "/" ++ show total ++ " tests passed"
  
  if passed == total
    then putStrLn "[PASS] All tests passed!"
    else putStrLn "[FAIL] Some tests failed"

printResult :: (String, Bool) -> IO ()
printResult (name, result) = 
  putStrLn $ (if result then "[PASS] " else "[FAIL] ") ++ name
