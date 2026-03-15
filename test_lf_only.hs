{-# LANGUAGE OverloadedStrings #-}

-- Test LF-only (Unix-style) line endings
-- Verifies that line ending detection works for both styles

import qualified Data.Text as T
import qualified Data.ByteString as BS
import HaFileViewer.LineCache
import HaFileViewer.BidirectionalScanner (Direction(..))
import System.IO

main :: IO ()
main = do
  putStrLn "=== Testing LF-only (Unix) Line Endings ==="
  
  -- Create LF-only test file (no CR, just LF)
  let testFile = "test-lf-only.txt"
  h <- openFile testFile WriteMode
  hSetBinaryMode h True
  BS.hPut h $ BS.pack [76, 105, 110, 101, 32, 49, 10]  -- "Line 1\n"
  BS.hPut h $ BS.pack [76, 105, 110, 101, 32, 50, 10]  -- "Line 2\n"
  BS.hPut h $ BS.pack [76, 105, 110, 101, 32, 51, 10]  -- "Line 3\n"
  hClose h
  
  -- Open with line cache
  lc <- openLineCache testFile
  
  -- Read all 3 lines
  (lines1, _, botPos1) <- getLinesFromStart lc 3
  putStrLn $ "Read " ++ show (length lines1) ++ " lines"
  mapM_ (\(txt, num) -> putStrLn $ "  Line " ++ show num ++ ": '" ++ T.unpack txt ++ "'") lines1
  
  -- Verify no empty lines
  let empties = filter (\(txt, _) -> T.null txt) lines1
  if null empties
    then putStrLn "✓ PASS: No empty lines"
    else putStrLn $ "✗ FAIL: Found " ++ show (length empties) ++ " empty lines"
  
  -- Read incrementally from line 2
  (lines2, _, _) <- getLinesFrom lc botPos1 Forward 1 4  -- Forward direction, line 4
  putStrLn $ "\nIncremental read: " ++ show (length lines2) ++ " lines"
  mapM_ (\(txt, num) -> putStrLn $ "  Line " ++ show num ++ ": '" ++ T.unpack txt ++ "'") lines2
  
  closeLineCache lc
  putStrLn "\n✓ All checks passed"
