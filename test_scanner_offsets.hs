#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import System.IO (withFile, IOMode(WriteMode), hSetBinaryMode, hPutStr)
import Control.Exception (bracket_)
import HaFileViewer.BidirectionalScanner
  ( Direction(..)
  , scanLinesWithOffsets
  , defaultChunkSize
  )

-- Create a simple test file with known CRLF line endings
-- File content: "A\r\nB\r\nC\r\n" (6 bytes after first line, 6 after second)
-- Actual byte positions:
--   A     \r    \n    B     \r    \n    C     \r    \n
--   0     1     2     3     4     5     6     7     8
testContent :: BS.ByteString
testContent = "A\r\nB\r\nC\r\n"

-- Expected offsets in file (BEFORE any processing):
-- - Line "A" should start at offset 0
-- - Line "B" should start at offset 3 (after "A\r\n" = 3 bytes)
-- - Line "C" should start at offset 6 (after "A\r\nB\r\n" = 6 bytes)

main :: IO ()
main = do
  putStrLn "=== Scanner Offset Direct Test ==="
  putStrLn ""
  
  putStrLn "Step 1: Create test content"
  putStrLn $ "Content (hex): " ++ show testContent
  putStrLn $ "Content (length): " ++ show (BS.length testContent) ++ " bytes"
  putStrLn ""
  
  putStrLn "Step 2: Display content breakdown"
  displayBytes testContent
  putStrLn ""
  
  putStrLn "Step 3: Expected offsets (from file structure)"
  putStrLn "  Line 0 (\"A\") should be at offset: 0"
  putStrLn "  Line 1 (\"B\") should be at offset: 3 (0 + 3 bytes for \"A\\r\\n\")"
  putStrLn "  Line 2 (\"C\") should be at offset: 6 (3 + 3 bytes for \"B\\r\\n\")"
  putStrLn ""
  
  putStrLn "Step 4: Call scanLinesWithOffsets directly"
  
  -- Create a simple readFn that returns the entire content on first call
  -- and empty on subsequent calls
  let readFn offset len = do
        putStrLn $ "  readFn called: offset=" ++ show offset ++ " len=" ++ show len
        if offset == 0
          then do
            putStrLn $ "  readFn returning all content (" ++ show (BS.length testContent) ++ " bytes)"
            return testContent
          else do
            putStrLn $ "  readFn returning empty (beyond EOF)"
            return BS.empty
  
  -- Call scanner for 3 lines
  result <- scanLinesWithOffsets Forward (fromIntegral $ BS.length testContent) readFn 3
  
  putStrLn ""
  putStrLn "Step 5: Check results"
  putStrLn $ "Number of lines returned: " ++ show (length result)
  putStrLn ""
  
  putStrLn "Step 6: Display returned lines and offsets"
  mapM_ (\(idx, (line, offset)) -> do
    putStrLn $ "  Line " ++ show idx ++ ": offset=" ++ show offset ++ " content=\"" ++ show line ++ "\"")
    (zip [0..] result)
  putStrLn ""
  
  putStrLn "Step 7: Verify correctness"
  verifyResults result

-- Display byte-by-byte breakdown of content
displayBytes :: BS.ByteString -> IO ()
displayBytes bs = do
  putStrLn "  Byte-by-byte breakdown:"
  mapM_ (\(idx, byte) -> do
    let char = if byte == 13 then "CR" else if byte == 10 then "LF" else [toEnum (fromEnum byte)]
    putStrLn $ "    [" ++ show idx ++ "] = " ++ show byte ++ " ('" ++ char ++ "')")
    (zip [0..] (BS.unpack bs))

-- Verify the returned offsets
verifyResults :: [(String, Integer)] -> IO ()
verifyResults result
  | length result < 3 = do
      putStrLn "ERROR: Expected 3 lines, got " ++ show (length result)
  | otherwise = do
      let (line0, offset0) = result !! 0
      let (line1, offset1) = result !! 1
      let (line2, offset2) = result !! 2
      
      putStrLn "Checking Line 0:"
      putStrLn $ "  Expected: line=\"A\", offset=0"
      putStrLn $ "  Got:      line=\"" ++ line0 ++ "\", offset=" ++ show offset0
      if offset0 == 0 && line0 == "A"
        then putStrLn "  ✓ PASS"
        else putStrLn "  ✗ FAIL"
      putStrLn ""
      
      putStrLn "Checking Line 1:"
      putStrLn $ "  Expected: line=\"B\", offset=3"
      putStrLn $ "  Got:      line=\"" ++ line1 ++ "\", offset=" ++ show offset1
      if offset1 == 3 && line1 == "B"
        then putStrLn "  ✓ PASS"
        else putStrLn "  ✗ FAIL"
      putStrLn ""
      
      putStrLn "Checking Line 2:"
      putStrLn $ "  Expected: line=\"C\", offset=6"
      putStrLn $ "  Got:      line=\"" ++ line2 ++ "\", offset=" ++ show offset2
      if offset2 == 6 && line2 == "C"
        then putStrLn "  ✓ PASS"
        else putStrLn "  ✗ FAIL"
      putStrLn ""
      
      -- Overall summary
      let allPass = offset0 == 0 && line0 == "A" &&
                    offset1 == 3 && line1 == "B" &&
                    offset2 == 6 && line2 == "C"
      if allPass
        then putStrLn "✓ All tests PASS - Scanner offsets are CORRECT"
        else putStrLn "✗ Some tests FAILED - Check scanner offset calculation"
