-- Test for Bug #4 and Bug #5: Direction-related bugs
-- Bug #4: Scrolling up after down causes line numbers to flip negative
-- Bug #5: After 'G', up arrow doesn't scroll

import HaFileViewer.LineCache
import HaFileViewer.BidirectionalScanner (Direction(..))
import Control.Exception (bracket)

main :: IO ()
main = do
  putStrLn "=== Testing Bug #4: Direction switch ==="
  putStrLn "Test: Start from beginning, scroll down, then up"
  bracket (openLineCache "test-sample.txt") closeLineCache $ \lc -> do
    -- Start from beginning
    (lines1, pos1) <- getLinesFromStart lc 3
    putStrLn $ "\nStep 1: Start from beginning (getLinesFromStart 3)"
    mapM_ (\(text, lineNum) -> putStrLn $ "  Line " ++ show lineNum ++ ": " ++ take 30 (show text)) lines1
    putStrLn $ "  Position: " ++ show pos1
    
    -- Scroll down several times
    (lines2, pos2) <- getLinesFrom lc pos1 Forward 3
    putStrLn $ "\nStep 2: Scroll down Forward 3 lines"
    mapM_ (\(text, lineNum) -> putStrLn $ "  Line " ++ show lineNum ++ ": " ++ take 30 (show text)) lines2
    putStrLn $ "  Position: " ++ show pos2
    
    (lines3, pos3) <- getLinesFrom lc pos2 Forward 3
    putStrLn $ "\nStep 3: Scroll down Forward 3 more lines"
    mapM_ (\(text, lineNum) -> putStrLn $ "  Line " ++ show lineNum ++ ": " ++ take 30 (show text)) lines3
    putStrLn $ "  Position: " ++ show pos3
    
    -- NOW THE KEY TEST: Scroll UP (Backward)
    (lines4, pos4) <- getLinesFrom lc pos3 Backward 3
    putStrLn $ "\nStep 4: Scroll UP Backward 3 lines (THE BUG TEST)"
    mapM_ (\(text, lineNum) -> putStrLn $ "  Line " ++ show lineNum ++ ": " ++ take 30 (show text)) lines4
    putStrLn $ "  Position: " ++ show pos4
    
    -- Check: Are line numbers still positive?
    let allPositive = all (\(_, ln) -> ln > 0) lines4
    putStrLn $ "\nBug #4 Result: " ++ if allPositive 
      then "PASS - Line numbers stayed POSITIVE"
      else "FAIL - Line numbers flipped to negative"
  
  putStrLn "\n\n=== Testing Bug #5: Inverted scrolling from end ==="
  putStrLn "Test: Jump to end with 'G', then scroll up"
  bracket (openLineCache "test-sample.txt") closeLineCache $ \lc -> do
    -- Start from end (like 'G' command)
    (lines1, pos1) <- getLinesFromEnd lc 3
    putStrLn $ "\nStep 1: Start from end (getLinesFromEnd 3)"
    mapM_ (\(text, lineNum) -> putStrLn $ "  Line " ++ show lineNum ++ ": " ++ take 30 (show text)) lines1
    putStrLn $ "  Position: " ++ show pos1
    
    -- NOW THE KEY TEST: Scroll UP (Backward) - should show PREVIOUS lines
    (lines2, pos2) <- getLinesFrom lc pos1 Backward 3
    putStrLn $ "\nStep 2: Scroll UP Backward 3 lines (THE BUG TEST)"
    mapM_ (\(text, lineNum) -> putStrLn $ "  Line " ++ show lineNum ++ ": " ++ take 30 (show text)) lines2
    putStrLn $ "  Position: " ++ show pos2
    
    -- Check: Did we get lines? Are they more negative?
    let gotLines = not (null lines2)
        moreNegative = if null lines2 || null lines1 
          then False 
          else let (_, firstNum1) = head lines1
                   (_, firstNum2) = head lines2
               in firstNum2 < firstNum1  -- Should be MORE negative
    
    putStrLn $ "\nBug #5 Result: " ++ 
      if gotLines && moreNegative
        then "PASS - Scrolling up from end works correctly"
        else "FAIL - " ++ 
             if not gotLines 
               then "No lines returned (inverted behavior)" 
               else "Line numbers not more negative"
