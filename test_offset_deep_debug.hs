{-# LANGUAGE OverloadedStrings #-}

-- Detailed debug: Print raw offset values and line content
-- Focus on understanding what's being returned for line 26

import HaFileViewer.LineCache
import HaFileViewer.BidirectionalScanner (Direction(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO
import Control.Monad (forM_)
import qualified Data.ByteString as BS

-- Create test file with 50 lines, all non-empty, with CR-LF
createTestFile :: FilePath -> IO ()
createTestFile path = withFile path WriteMode $ \h -> do
  hSetBinaryMode h True
  forM_ [1..50] $ \i -> do
    -- Each line has content with line number, guaranteed non-empty
    let line = "Line " ++ show i ++ " has content here to make it non-empty"
    hPutStr h (line ++ "\r\n")

-- Helper to show LinePosition details (workaround since lpOffset not exported)
showLinePosition :: LinePosition -> String
showLinePosition lp = "LinePosition{origin=" ++ show (lpOrigin lp) ++ "}"

main :: IO ()
main = do
  putStrLn "=== DEEP OFFSET DEBUG - Line 26 Problem Analysis ==="
  putStrLn ""
  
  -- Create test file
  let path = "test-incremental.txt"
  createTestFile path
  putStrLn "Created test file with 50 non-empty lines (CR-LF endings)"
  putStrLn ""
  
  -- Examine raw file to understand line 26
  putStrLn "=== Raw File Analysis ==="
  fileContent <- BS.readFile path
  putStrLn $ "Total file size: " ++ show (BS.length fileContent) ++ " bytes"
  
  -- Calculate where line 26 should start
  putStrLn ""
  putStrLn "Expected line offsets (assuming uniform line length):"
  putStrLn "  Line 1: offset 0"
  putStrLn "  Line 2: offset ~68 (line length 66 + CRLF 2)"
  putStrLn "  ..."
  putStrLn "  Line 26: offset ~1700 (25 * 68)"
  
  putStrLn ""
  putStrLn "=== Cache Test ==="
  
  -- Open cache
  cache <- openLineCache path
  
  -- Read initial 25 lines
  putStrLn "Step 1: Read lines 1-25"
  (initial, topPos1, botPos1) <- getLinesFromStart cache 25
  putStrLn $ "  Got " ++ show (length initial) ++ " lines"
  putStrLn $ "  botPos1 origin: " ++ show (lpOrigin botPos1)
  putStrLn ""
  
  -- Now read line 26
  putStrLn "Step 2: Read line 26 from botPos1"
  (line26Result, topPos26, botPos26) <- getLinesFrom cache botPos1 Forward 1 26
  
  putStrLn $ "  getLinesFrom returned:"
  putStrLn $ "    Result count: " ++ show (length line26Result)
  
  if null line26Result
    then putStrLn "    ERROR: Got empty result list!"
    else do
      let (text, lineNum) = head line26Result
      putStrLn $ "    Line number: " ++ show lineNum
      putStrLn $ "    Text length: " ++ show (T.length text) ++ " chars"
      putStrLn $ "    Text content: '" ++ T.unpack (T.take 100 text) ++ "'"
      putStrLn $ "    Is empty: " ++ show (T.null text)
      putStrLn $ "    Text bytes: " ++ show (TE.encodeUtf8 text)
  
  putStrLn ""
  putStrLn $ "  topPos26 origin: " ++ show (lpOrigin topPos26)
  putStrLn $ "  botPos26 origin: " ++ show (lpOrigin botPos26)
  
  putStrLn ""
  putStrLn "Step 3: Read line 27 from botPos26"
  (line27Result, topPos27, botPos27) <- getLinesFrom cache botPos26 Forward 1 27
  
  putStrLn $ "  getLinesFrom returned:"
  putStrLn $ "    Result count: " ++ show (length line27Result)
  
  if null line27Result
    then putStrLn "    ERROR: Got empty result list!"
    else do
      let (text, lineNum) = head line27Result
      putStrLn $ "    Line number: " ++ show lineNum
      putStrLn $ "    Text length: " ++ show (T.length text) ++ " chars"
      putStrLn $ "    Text content: '" ++ T.unpack (T.take 100 text) ++ "'"
      putStrLn $ "    Is empty: " ++ show (T.null text)
  
  putStrLn ""
  putStrLn "=== Analysis ==="
  putStrLn "Hypothesis: LinePosition between getLinesFromStart and first getLinesFrom"
  putStrLn "is being calculated incorrectly, causing first incremental read to get"
  putStrLn "wrong line or empty line."
  putStrLn ""
  putStrLn "The botPos1 from getLinesFromStart should point EXACTLY after line 25"
  putStrLn "So the next getLinesFrom should return line 26."
  putStrLn ""
  putStrLn "If line 26 comes back empty, the offset in botPos1 is incorrect."
  
  closeLineCache cache
