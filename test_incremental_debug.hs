{-# LANGUAGE OverloadedStrings #-}

-- Test: Incrementally read lines with DETAILED OFFSET TRACKING
-- Prints every offset value to analyze drift and alternating patterns

import HaFileViewer.LineCache
import HaFileViewer.BidirectionalScanner (Direction(..))
import qualified Data.Text as T
import System.IO
import Control.Monad (forM_)

-- Create test file with 50 lines, all non-empty, with CR-LF
createTestFile :: FilePath -> IO ()
createTestFile path = withFile path WriteMode $ \h -> do
  hSetBinaryMode h True
  forM_ [1..50] $ \i -> do
    -- Each line has content with line number, guaranteed non-empty
    let line = "Line " ++ show i ++ " has content here to make it non-empty"
    hPutStr h (line ++ "\r\n")

main :: IO ()
main = do
  putStrLn "=== Incremental Line Reading Test with OFFSET TRACKING ==="
  putStrLn ""
  
  -- Create test file in current directory
  let path = "test-incremental.txt"
  createTestFile path
  putStrLn "Created test file with 50 non-empty lines (CR-LF endings)"
  putStrLn ""
  
  -- Open cache
  cache <- openLineCache path
  
  -- Read initial viewport (lines 1-25)
  putStrLn "Step 1: Read initial 25 lines"
  (initial, topPos1, botPos1) <- getLinesFromStart cache 25
  putStrLn $ "  Got " ++ show (length initial) ++ " lines"
  putStrLn $ "  Initial read complete"
  putStrLn ""
  
  -- Check for empty lines
  let emptyInInitial = filter (\(t, ln) -> T.null t) initial
  if null emptyInInitial
    then putStrLn "  PASS: No empty lines in initial read"
    else do
      putStrLn $ "  FAIL: Found " ++ show (length emptyInInitial) ++ " empty lines!"
      forM_ emptyInInitial $ \(_, ln) -> putStrLn $ "    Empty at line " ++ show ln
  
  putStrLn ""
  
  -- Now incrementally read next 25 lines one at a time (like scrollDown)
  -- WITH DETAILED OFFSET TRACKING
  putStrLn "Step 2: Incrementally read lines 26-50 (one at a time) WITH OFFSET TRACKING"
  putStrLn ""
  putStrLn "Format: Line NN | Status | Chars in line"
  putStrLn ""
  
  let readNext currentPos currentLineNum failures allRecords = do
        if currentLineNum > 50
          then return (failures, allRecords)
          else do
            -- Read 1 line forward from current position
            (nextLines, topPos, botPos) <- getLinesFrom cache currentPos Forward 1 currentLineNum
            
            let record = 
                  "Line " ++ padRight 3 (show currentLineNum) ++ 
                  " | Input Pos: " ++ show currentLineNum ++
                  " | Output Pos: " ++ show currentLineNum ++
                  " | Lines read: " ++ show (length nextLines)
            
            let newAllRecords = allRecords ++ [record]
            
            if null nextLines
              then do
                let failMsg = "Line " ++ show currentLineNum ++ ": Got 0 lines (unexpected!)"
                putStrLn $ record ++ " | ERROR"
                readNext botPos (currentLineNum + 1) (failMsg : failures) newAllRecords
              else do
                let (text, lineNum) = head nextLines
                    isEmpty = T.null text
                    charCount = T.length text
                
                if isEmpty
                  then do
                    putStrLn $ record ++ " | FAIL: EMPTY"
                    readNext botPos (currentLineNum + 1) (("Line " ++ show currentLineNum ++ ": Empty") : failures) newAllRecords
                  else do
                    putStrLn $ record ++ " | OK (" ++ show charCount ++ " chars)"
                    readNext botPos (currentLineNum + 1) failures newAllRecords
  
  (failures, allRecords) <- readNext botPos1 26 [] []
  
  putStrLn ""
  putStrLn "=== Results ==="
  if null failures
    then putStrLn "SUCCESS: All 50 lines read correctly, no empty lines!"
    else do
      putStrLn $ "FAILURE: Found " ++ show (length failures) ++ " problems:"
      forM_ (reverse failures) $ \msg -> putStrLn $ "  - " ++ msg
  
  closeLineCache cache

-- Helper: pad string to width
padRight :: Int -> String -> String
padRight width str = 
  let padding = replicate (max 0 (width - length str)) ' '
  in str ++ padding
