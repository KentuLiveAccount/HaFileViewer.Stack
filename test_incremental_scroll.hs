{-# LANGUAGE OverloadedStrings #-}

-- Test: Incrementally read lines one at a time (like scrollDown)
-- File has NO empty lines, so any empty line returned is a BUG

import HaFileViewer.LineCache
import HaFileViewer.BidirectionalScanner (Direction(..))
import qualified Data.Text as T
import System.IO.Temp (withSystemTempFile)
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
  putStrLn "=== Incremental Line Reading Test (like scrollDown) ==="
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
  
  -- Check for empty lines
  let emptyInInitial = filter (\(t, ln) -> T.null t) initial
  if null emptyInInitial
    then putStrLn "  PASS: No empty lines in initial read"
    else do
      putStrLn $ "  FAIL: Found " ++ show (length emptyInInitial) ++ " empty lines!"
      forM_ emptyInInitial $ \(_, ln) -> putStrLn $ "    Empty at line " ++ show ln
  
  putStrLn ""
  
  -- Now incrementally read next 25 lines one at a time (like scrollDown)
  putStrLn "Step 2: Incrementally read lines 26-50 (one at a time)"
  let readNext currentPos currentLineNum failures = do
        if currentLineNum > 50
          then return failures
          else do
            -- Read 1 line forward from current position
            (nextLines, topPos, botPos) <- getLinesFrom cache currentPos Forward 1 currentLineNum
            
            if null nextLines
              then do
                putStrLn $ "  Line " ++ show currentLineNum ++ ": Got 0 lines (unexpected!)"
                return (("Line " ++ show currentLineNum ++ ": Got 0 lines") : failures)
              else do
                let (text, lineNum) = head nextLines
                    isEmpty = T.null text
                    charCount = T.length text
                
                if isEmpty
                  then do
                    putStrLn $ "  Line " ++ show currentLineNum ++ ": FAIL - EMPTY LINE RETURNED"
                    readNext botPos (currentLineNum + 1) (("Line " ++ show currentLineNum ++ ": Empty") : failures)
                  else do
                    -- Only print every 5th line to reduce output
                    if currentLineNum `mod` 5 == 0
                      then putStrLn $ "  Line " ++ show currentLineNum ++ ": PASS (" ++ show charCount ++ " chars)"
                      else return ()
                    readNext botPos (currentLineNum + 1) failures
  
  failures <- readNext botPos1 26 []
  
  putStrLn ""
  putStrLn "=== Results ==="
  if null failures
    then putStrLn "SUCCESS: All 50 lines read correctly, no empty lines!"
    else do
      putStrLn $ "FAILURE: Found " ++ show (length failures) ++ " problems:"
      forM_ (reverse failures) $ \msg -> putStrLn $ "  - " ++ msg
  
  closeLineCache cache
