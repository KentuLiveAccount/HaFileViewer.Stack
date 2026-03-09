{-# LANGUAGE OverloadedStrings #-}

-- Unit test: Does getLinesFrom return correct positions?

import HaFileViewer.LineCache
import HaFileViewer.BidirectionalScanner (Direction(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import System.IO

testFile :: FilePath
testFile = "test-onenote.log"

main :: IO ()
main = do
  putStrLn "=== Testing Position Calculation Bug ==="
  putStrLn ""
  
  cache <- openLineCache testFile
  
  -- Get line 31 to examine
  putStrLn "Getting line 31 from scratch..."
  (lines31, _, botPos31) <- getLinesFromStart cache 31
  let (text31, _) = last lines31
  
  putStrLn $ "Line 31 text: " ++ T.unpack (T.take 80 text31)
  putStrLn $ "Line 31 length (characters): " ++ show (T.length text31)
  putStrLn $ "Line 31 length (bytes): " ++ show (BS.length $ TE.encodeUtf8 text31)
  putStrLn ""
  
  -- Now read line 32 using the bottomPos from line 31
  putStrLn "Reading line 32 using bottomPos from line 31..."
  (lines32, topPos32, botPos32) <- getLinesFrom cache botPos31 Forward 1 32
  
  if null lines32
    then putStrLn "✗ FAIL: Got 0 lines"
    else do
      let (text32, lineNum32) = head lines32
      putStrLn $ "Line " ++ show lineNum32 ++ ": len=" ++ show (T.length text32) 
              ++ " empty=" ++ show (T.null text32)
      if T.null text32
        then do
          putStrLn "✗ FAIL: Line 32 is EMPTY"
          putStrLn ""
          putStrLn "HYPOTHESIS: bottomOffset calculation is wrong!"
          putStrLn "  Line 427 in LineCache.lhs:"
          putStrLn "    lastOff + fromIntegral (T.length lastText) + 1"
          putStrLn ""
          putStrLn "  T.length gives CHARACTER count, not BYTE count!"
          putStrLn "  For UTF-8 with multi-byte chars, this is WRONG."
          putStrLn ""
          putStrLn "  Should be:"
          putStrLn "    lastOff + fromIntegral (BS.length $ TE.encodeUtf8 lastText) + 1"
        else
          putStrLn $ "✓ Line 32: [" ++ T.unpack (T.take 60 text32) ++ "]"
  
  closeLineCache cache
  
  putStrLn ""
  putStrLn "=== Manual Verification ==="
  putStrLn "Reading raw bytes from file to verify..."
  
  -- Read lines 30-33 directly from file to compare
  withFile testFile ReadMode $ \h -> do
    contents <- hGetContents h
    let fileLines = lines contents
    putStrLn $ "Line 31 (file): " ++ take 80 (fileLines !! 30)
    putStrLn $ "Line 32 (file): " ++ take 80 (fileLines !! 31)
    putStrLn $ "Line 33 (file): " ++ take 80 (fileLines !! 32)
