{-# LANGUAGE OverloadedStrings #-}

-- Byte-level analysis: Read exact bytes from file and compare with what cache returns

import HaFileViewer.LineCache
import HaFileViewer.BidirectionalScanner (Direction(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import System.IO
import Control.Monad (forM_)

-- Create test file - EXACTLY 3 lines to make analysis trivial
createSimpleTestFile :: FilePath -> IO ()
createSimpleTestFile path = withFile path WriteMode $ \h -> do
  hSetBinaryMode h True
  hPutStr h "Line 1 content\r\n"
  hPutStr h "Line 2 content\r\n"
  hPutStr h "Line 3 content\r\n"

main :: IO ()
main = do
  putStrLn "=== BYTE-LEVEL OFFSET ANALYSIS ==="
  putStrLn ""
  
  let path = "test-simple.txt"
  createSimpleTestFile path
  
  -- Read and display raw bytes
  putStrLn "Step 1: Raw file content and byte positions"
  content <- BS.readFile path
  putStrLn $ "File size: " ++ show (BS.length content) ++ " bytes"
  putStrLn ""
  putStrLn "Raw content:"
  putStrLn $ T.unpack (TE.decodeUtf8 content)
  
  -- Line boundaries
  putStrLn ""
  putStrLn "Expected line boundaries:"
  putStrLn "  Line 1: bytes 0-14 (15 bytes for 'Line 1 content')"
  putStrLn "  CRLF:   bytes 15-16 (2 bytes for \\r\\n)"
  putStrLn "  Line 2: bytes 17-31 (15 bytes for 'Line 2 content')"
  putStrLn "  CRLF:   bytes 32-33"
  putStrLn "  Line 3: bytes 34-48 (15 bytes for 'Line 3 content')"
  putStrLn "  CRLF:   bytes 49-50"
  
  putStrLn ""
  putStrLn "Step 2: Cache behavior"
  putStrLn ""
  
  cache <- openLineCache path
  
  -- Read all 3 lines
  (lines123, topPos, botPos) <- getLinesFromStart cache 3
  putStrLn $ "getLinesFromStart cache 3:"
  putStrLn $ "  Got " ++ show (length lines123) ++ " lines"
  forM_ lines123 $ \(text, lineNum) -> do
    putStrLn $ "  Line " ++ show lineNum ++ ": '" ++ T.unpack text ++ "'"
  
  putStrLn ""
  putStrLn "Step 3: Try to read BEYOND line 3"
  putStrLn "(This will show what offset botPos is actually pointing to)"
  
  (resultBeyond, _, _) <- getLinesFrom cache botPos Forward 1 4
  putStrLn $ "getLinesFrom cache botPos Forward 1 4:"
  putStrLn $ "  Got " ++ show (length resultBeyond) ++ " lines"
  if null resultBeyond
    then putStrLn "  (No more lines - botPos points past end, CORRECT)"
    else do
      let (text, lineNum) = head resultBeyond
      putStrLn $ "  Line " ++ show lineNum ++ ": '" ++ T.unpack text ++ "'"
      putStrLn $ "  (Got a line - botPos is NOT past end!)"
      putStrLn $ "  Text length: " ++ show (T.length text)
  
  putStrLn ""
  putStrLn "Step 4: Read just line 1, then continue"
  
  (lines1, top1, bot1) <- getLinesFromStart cache 1
  putStrLn $ "getLinesFromStart cache 1:"
  putStrLn $ "  Got " ++ show (length lines1) ++ " line"
  let (text1, _) = head lines1
  putStrLn $ "  Content: '" ++ T.unpack text1 ++ "'"
  
  putStrLn ""
  putStrLn "Continue from bot1:"
  (line2Result, _, bot2) <- getLinesFrom cache bot1 Forward 1 2
  if null line2Result
    then putStrLn "  Got 0 lines (ERROR - should get line 2)"
    else do
      let (text2, ln2) = head line2Result
      putStrLn $ "  Line " ++ show ln2 ++ ": '" ++ T.unpack text2 ++ "'"
      if T.null text2
        then putStrLn $ "  (EMPTY - offset is wrong!)"
        else if T.head text2 == 'i'  -- 'Line' missing the 'L'
             then putStrLn $ "  (MISSING FIRST CHAR - offset is +1!)"
             else putStrLn $ "  (OK)"
  
  closeLineCache cache

padLeft :: Int -> String -> String
padLeft width str =
  let padding = replicate (max 0 (width - length str)) ' '
  in padding ++ str
