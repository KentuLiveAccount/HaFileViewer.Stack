{-# LANGUAGE OverloadedStrings #-}

-- Debug test to understand offset behavior

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HaFileViewer.LineCache
import HaFileViewer.BidirectionalScanner (Direction(..))
import Control.Exception (bracket)
import System.IO (writeFile)

main :: IO ()
main = do
  let testFile = "test-offset-debug.txt"
  -- "Line 1\n" = 7 bytes
  -- "Line 2\n" = 7 bytes
  -- "Line 3\n" = 7 bytes
  -- Total first 3 lines = 21 bytes (but position shows 20)
  writeFile testFile "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n"
  
  bracket (openLineCache testFile) closeLineCache $ \lc -> do
    putStrLn "=== Test: Read first 2 lines ==="
    (lines1, pos1) <- getLinesFromStart lc 2
    putStrLn $ "Lines: " ++ show lines1
    putStrLn $ "Position: " ++ show pos1
    putStrLn ""
    
    putStrLn "=== Test: Continue from position (Forward) ==="
    (lines2, pos2) <- getLinesFrom lc pos1 Forward 2
    putStrLn $ "Lines: " ++ show lines2
    putStrLn $ "Position: " ++ show pos2
    putStrLn ""
    
    putStrLn "Expected:"
    putStrLn "  First 2 lines: Line 1, Line 2"
    putStrLn "  Next 2 lines: Line 3, Line 4"
    putStrLn "  But we might be getting partial content"
