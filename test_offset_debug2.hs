{-# LANGUAGE OverloadedStrings #-}

-- Debug: Check offset calculation from getLinesFromStart

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
  putStrLn "=== Checking Offset Calculation ==="
  putStrLn ""
  
  cache <- openLineCache testFile
  
  -- Get first 30 lines
  (lines30, topPos30, botPos30) <- getLinesFromStart cache 30
  
  let line30 = lines30 !! 29  -- Last line (0-indexed)
      (text30, lineNum30) = line30
      charLen30 = T.length text30
      byteLen30 = BS.length $ TE.encodeUtf8 text30
      
  putStrLn $ "Line 30 from cache:"
  putStrLn $ "  Line number: " ++ show lineNum30
  putStrLn $ "  Character length: " ++ show charLen30
  putStrLn $ "  Byte length: " ++ show byteLen30
  putStrLn $ "  Text: " ++ T.unpack (T.take 80 text30)
  
  let LinePosition botOff _ = botPos30
  putStrLn ""
  putStrLn $ "bottomPosition offset: " ++ show (botOff :: Integer)
  
  -- Manually calculate what it should be
  putStrLn ""
  putStrLn "Manual calculation:"
  putStrLn "  Need to find the byte offset where line 30 STARTS"
  putStrLn "  Then add byte length of line 30 + 1 (newline)"
  
  -- Read file directly to get byte offset of line 31
  withFile testFile ReadMode $ \h -> do
    -- Read lines until we've consumed 30 lines
    let readLines n offset = do
          if n == 0
            then return offset
            else do
              line <- hGetLine h
              let lineBytes = BS.length $ TE.encodeUtf8 $ T.pack line
              -- +1 for newline
              readLines (n-1) (offset + fromIntegral lineBytes + 1)
    
    actualOffset31 <- readLines 30 0
    putStrLn $ "  Actual offset of line 31 (from file): " ++ show actualOffset31
    putStrLn $ "  Difference: " ++ show (botOff - actualOffset31)
    
    if botOff == actualOffset31
      then putStrLn "  ✓ Offsets MATCH!"
      else putStrLn $ "  ✗ Offsets DON'T MATCH (off by " ++ show (abs (botOff - actualOffset31)) ++ " bytes)"
  
  closeLineCache cache
