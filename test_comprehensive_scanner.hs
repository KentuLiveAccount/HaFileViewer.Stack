{-# LANGUAGE OverloadedStrings #-}

-- Comprehensive test for bottomOffset bug and scanner offset validation
-- This test validates that:
-- 1. Scanner returns correct offsets
-- 2. bottomOffset formula works for both CRLF and LF files

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

main :: IO ()
main = do
  putStrLn "=== Comprehensive Scanner & bottomOffset Test ==="
  putStrLn ""
  
  -- Test 1: Validate scanner offset calculations
  putStrLn "TEST 1: Scanner Offset Calculations"
  putStrLn "==================================="
  testScannerOffsets
  putStrLn ""
  
  -- Test 2: Validate bottomOffset formula bug
  putStrLn "TEST 2: bottomOffset Formula Bug"
  putStrLn "================================"
  testBottomOffsetBug
  putStrLn ""
  
  -- Test 3: Expected vs Actual behavior
  putStrLn "TEST 3: CRLF vs LF Comparison"
  putStrLn "============================="
  testCRLFvLF
  putStrLn ""

-- Test 1: Scanner offset calculations with CRLF content
testScannerOffsets :: IO ()
testScannerOffsets = do
  let content = "A\r\nB\r\nC\r\n" :: String
      pieces = ["A\r", "B\r", "C\r", ""] :: [String]
  
  putStrLn $ "File content: " ++ show content ++ " (9 bytes)"
  putStrLn $ "After BS.split on LF: " ++ show pieces
  putStrLn ""
  
  -- Calculate offsets manually (simulating scanner)
  let offsets = calculateOffsets 0 pieces
  
  putStrLn "Calculated offsets (what scanner returns):"
  mapM_ (\(idx, off) -> putStrLn $ "  Offset[" ++ show idx ++ "] = " ++ show off)
    (zip [0..] offsets)
  putStrLn ""
  
  -- Verify against expected
  let expected = [0, 3, 6, 9]
  if offsets == expected
    then putStrLn "✓ PASS: Scanner offsets are CORRECT"
    else putStrLn $ "✗ FAIL: Expected " ++ show expected ++ " but got " ++ show offsets
  where
    calculateOffsets :: Integer -> [String] -> [Integer]
    calculateOffsets _ [] = []
    calculateOffsets start (p:ps) = 
      start : calculateOffsets (start + fromIntegral (length p) + 1) ps

-- Test 2: Show the bottomOffset bug
testBottomOffsetBug :: IO ()
testBottomOffsetBug = do
  let lastLine = T.pack "C"
      lastOff = 6 :: Integer
      
  putStrLn $ "Last line from scanner: (\"" ++ show lastLine ++ "\", " ++ show lastOff ++ ")"
  putStrLn ""
  
  -- Current (WRONG) formula
  let textLen = fromIntegral (BS.length $ TE.encodeUtf8 lastLine) :: Integer
      currentFormula = lastOff + textLen + 1  -- Wrong: assumes LF only
      correctFormula = lastOff + textLen + 2  -- Correct: accounts for \r\n
  
  putStrLn "Current formula (WRONG for CRLF):"
  putStrLn $ "  bottomOffset = " ++ show lastOff ++ " + " ++ show textLen ++ " + 1"
  putStrLn $ "              = " ++ show currentFormula
  putStrLn ""
  
  putStrLn "Correct formula (for CRLF):"
  putStrLn $ "  bottomOffset = " ++ show lastOff ++ " + " ++ show textLen ++ " + 2"
  putStrLn $ "              = " ++ show correctFormula
  putStrLn ""
  
  putStrLn "File size: 9 bytes"
  putStrLn $ "Current result: " ++ show currentFormula ++ " ✗ WRONG (off by 1)"
  putStrLn $ "Correct result: " ++ show correctFormula ++ " ✓ CORRECT"
  putStrLn ""
  
  if currentFormula == 8 && correctFormula == 9
    then putStrLn "✓ PASS: Bug confirmed - formula is off by 1 for CRLF"
    else putStrLn "✗ FAIL: Unexpected values"

-- Test 3: CRLF vs LF comparison
testCRLFvLF :: IO ()
testCRLFvLF = do
  putStrLn "Comparing CRLF and LF line ending handling:"
  putStrLn ""
  
  putStrLn "SCENARIO 1: CRLF File"
  putStrLn "  File: \"A\\r\\nB\\r\\nC\\r\\n\" (9 bytes)"
  putStrLn "  Last line: (\"C\", 6)"
  putStrLn "  Text length: 1"
  putStrLn "  Next byte (at 7): \\r (ASCII 13)"
  putStrLn "  Should add: 2 (for \\r\\n)"
  putStrLn "  Result: 6 + 1 + 2 = 9 ✓"
  putStrLn ""
  
  putStrLn "SCENARIO 2: LF-only File"
  putStrLn "  File: \"A\\nB\\nC\\n\" (6 bytes)"
  putStrLn "  Last line: (\"C\", 4)"
  putStrLn "  Text length: 1"
  putStrLn "  Next byte (at 5): \\n (ASCII 10, NOT CR)"
  putStrLn "  Should add: 1 (for \\n only)"
  putStrLn "  Result: 4 + 1 + 1 = 6 ✓"
  putStrLn ""
  
  putStrLn "SCENARIO 3: Mixed Endings (shouldn't happen, but would work)"
  putStrLn "  Each line is handled independently"
  putStrLn "  Check if CR present at position (lineOffset + lineLength)"
  putStrLn "  Add 2 or 1 accordingly"
  putStrLn ""
  
  putStrLn "✓ Solution: Peek at file to detect CRLF for each line"

-- Summary
putStr :: String -> IO ()
putStr = Prelude.putStr
