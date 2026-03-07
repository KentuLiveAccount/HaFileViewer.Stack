{-# LANGUAGE OverloadedStrings #-}

-- Integration test for Phase 1: New LineCache API
-- Tests getLinesFromStart, getLinesFromEnd, and getLinesFrom

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as BS
import HaFileViewer.LineCache
import HaFileViewer.BidirectionalScanner (Direction(..))
import Control.Exception (bracket)

main :: IO ()
main = do
  putStrLn "Phase 1 Integration Tests"
  putStrLn "========================="
  putStrLn ""
  
  -- Create a test file with Unix line endings
  let testFile = "test-phase1.txt"
  BS.writeFile testFile $ BS.unlines 
    [ "Line 1"
    , "Line 2"
    , "Line 3"
    , "Line 4"
    , "Line 5"
    , "Line 6"
    , "Line 7"
    , "Line 8"
    , "Line 9"
    , "Line 10"
    ]
  
  bracket (openLineCache testFile) closeLineCache $ \lc -> do
    -- Test 1: getLinesFromStart
    putStrLn "Test 1: getLinesFromStart (read 3 lines)"
    (lines1, pos1) <- getLinesFromStart lc 3
    putStrLn $ "  Lines read: " ++ show (length lines1)
    mapM_ (\(text, lineNum) -> putStrLn $ "    Line " ++ show lineNum ++ ": " ++ T.unpack text) lines1
    putStrLn $ "  Position: " ++ show pos1
    putStrLn ""
    
    -- Test 2: getLinesFrom forward (continue from pos1)
    putStrLn "Test 2: getLinesFrom Forward (continue reading 2 more)"
    (lines2, pos2) <- getLinesFrom lc pos1 Forward 2
    putStrLn $ "  Lines read: " ++ show (length lines2)
    mapM_ (\(text, lineNum) -> putStrLn $ "    Line " ++ show lineNum ++ ": " ++ T.unpack text) lines2
    putStrLn $ "  Position: " ++ show pos2
    putStrLn ""
    
    -- Test 3: getLinesFromEnd
    putStrLn "Test 3: getLinesFromEnd (read last 3 lines)"
    (lines3, pos3) <- getLinesFromEnd lc 3
    putStrLn $ "  Lines read: " ++ show (length lines3)
    mapM_ (\(text, lineNum) -> putStrLn $ "    Line " ++ show lineNum ++ ": " ++ T.unpack text) lines3
    putStrLn $ "  Position: " ++ show pos3
    putStrLn ""
    
    -- Test 4: getLinesFrom backward (continue from pos3)
    putStrLn "Test 4: getLinesFrom Backward (read 2 more backward)"
    (lines4, pos4) <- getLinesFrom lc pos3 Backward 2
    putStrLn $ "  Lines read: " ++ show (length lines4)
    mapM_ (\(text, lineNum) -> putStrLn $ "    Line " ++ show lineNum ++ ": " ++ T.unpack text) lines4
    putStrLn $ "  Position: " ++ show pos4
    putStrLn ""
    
    putStrLn "All tests completed!"
    putStrLn ""
    
    -- Verify expected behavior
    putStrLn "Verification:"
    let test1Pass = length lines1 == 3 && 
                   map snd lines1 == [1, 2, 3] &&
                   map fst lines1 == ["Line 1", "Line 2", "Line 3"]
    putStrLn $ "  Test 1 " ++ (if test1Pass then "[PASS]" else "[FAIL]")
    
    let test3Pass = length lines3 == 3 &&
                   map snd lines3 == [-3, -2, -1]
    putStrLn $ "  Test 3 " ++ (if test3Pass then "[PASS]" else "[FAIL]")
    
    if test1Pass && test3Pass
      then putStrLn "\n[PASS] Phase 1 API works correctly!"
      else putStrLn "\n[FAIL] Some tests failed"
