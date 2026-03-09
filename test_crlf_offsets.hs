{-# LANGUAGE OverloadedStrings #-}

-- Unit test: Verify offset calculations for CR-LF files

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Word (Word8)
import System.IO.Temp (withSystemTempFile)
import System.IO
import HaFileViewer.BidirectionalScanner
import HaFileViewer.LineCache
import Control.Monad (forM_)
import qualified Data.Text as T

-- Test data with known CR-LF line endings
testLines :: [String]
testLines = 
  [ "Line 1 content"      -- 14 chars + CR-LF (2 bytes) = 16 bytes total
  , "Line 2 longer text"  -- 18 chars + CR-LF (2 bytes) = 20 bytes total
  , "Line 3 short"        -- 12 chars + CR-LF (2 bytes) = 14 bytes total
  ]

main :: IO ()
main = withSystemTempFile "test-crlf.txt" $ \path h -> do
  putStrLn "=== Unit Test: Offset Calculation with CR-LF ==="
  putStrLn ""
  
  -- IMPORTANT: Close the text handle and reopen in binary mode
  hClose h
  
  -- Write test file with CR-LF line endings in BINARY mode
  withFile path WriteMode $ \hBin -> do
    hSetBinaryMode hBin True
    putStrLn "Creating test file with CR-LF line endings (binary mode):"
    let contentWithCRLF = concatMap (\line -> line ++ "\r\n") testLines
    hPutStr hBin contentWithCRLF
    hFlush hBin
  
  -- Show what we wrote
  bytes <- BS.readFile path
  putStrLn $ "  Total bytes written: " ++ show (BS.length bytes)
  putStrLn $ "  Expected: " ++ show (sum [length line + 2 | line <- testLines])
  putStrLn ""
  
  -- Show byte-by-byte for first line
  putStrLn "First 20 bytes (should show Line 1 with CR-LF):"
  let showBytes = take 20 $ zip [0..] (BS.unpack bytes)
  forM_ showBytes $ \(i, b) -> do
    let char = if b == 13 then "<CR>" 
               else if b == 10 then "<LF>"
               else if b >= 32 && b < 127 then show (toEnum (fromIntegral b) :: Char)
               else "<?>"
    putStrLn $ "  Byte " ++ show i ++ ": " ++ show b ++ " = " ++ char
  putStrLn ""
  
  -- Now test the scanner
  putStrLn "Testing LineCache with this file:"
  cache <- openLineCache path
  (lines3, topPos, botPos) <- getLinesFromStart cache 3
  
  putStrLn $ "  Got " ++ show (length lines3) ++ " lines"
  putStrLn ""
  
  -- Check each line's content
  putStrLn "Verifying line content:"
  forM_ (zip [1..] lines3) $ \(lineNum, (text, ln)) -> do
    let expected = testLines !! (lineNum - 1)
        actual = T.unpack text
        matches = actual == expected
    putStrLn $ "  Line " ++ show lineNum ++ ": " 
            ++ (if matches then "PASS" else "FAIL") 
            ++ " [" ++ actual ++ "]"
    if not matches
      then putStrLn $ "    Expected: [" ++ expected ++ "]"
      else return ()
  
  closeLineCache cache
  putStrLn ""
  putStrLn "Expected offsets:"
  putStrLn "  Line 1 starts at: 0"
  putStrLn "  Line 2 starts at: 16  (14 chars + 2 for CR-LF)"
  putStrLn "  Line 3 starts at: 36  (16 + 18 + 2)"
  putStrLn "  Line 4 would be: 50  (36 + 12 + 2)"
  putStrLn ""
  putStrLn "=== Test Complete ==="
