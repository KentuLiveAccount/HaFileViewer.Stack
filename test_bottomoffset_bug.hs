{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Test the bottomOffset formula with our known example

main :: IO ()
main = do
  putStrLn "=== Testing bottomOffset Formula Bug ==="
  putStrLn ""
  
  -- Our test file: "A\r\nB\r\nC\r\n" (9 bytes)
  -- Scanner returns: [("A", 0), ("B", 3), ("C", 6)]
  
  let lines = [(T.pack "A", 0), (T.pack "B", 3), (T.pack "C", 6)] :: [(T.Text, Integer)]
  
  putStrLn "Lines with offsets returned by scanner:"
  mapM_ (\(line, offset) -> putStrLn $ "  (\"" ++ show line ++ "\", " ++ show offset ++ ")")
    lines
  putStrLn ""
  
  -- Apply the formula from LineCache
  let (lastText, lastOff) = last lines
      textBytes = BS.length . TE.encodeUtf8 $ lastText
      bottomOffset = lastOff + fromIntegral textBytes + 1 :: Integer
  
  putStrLn "LineCache formula calculation:"
  putStrLn $ "  lastText = " ++ show lastText
  putStrLn $ "  lastOff = " ++ show lastOff
  putStrLn $ "  textBytes = BS.length (TE.encodeUtf8 lastText)"
  putStrLn $ "            = BS.length \"C\""
  putStrLn $ "            = " ++ show textBytes
  putStrLn $ "  bottomOffset = " ++ show lastOff ++ " + " ++ show textBytes ++ " + 1"
  putStrLn $ "              = " ++ show bottomOffset
  putStrLn ""
  
  putStrLn "But wait! The lastOff is the BYTE OFFSET of the last line START!"
  putStrLn "  - lastOff = 6 means 'C' starts at byte 6"
  putStrLn "  - 'C' is 1 byte long"
  putStrLn "  - So 'C' occupies byte 6"
  putStrLn "  - The byte AFTER 'C' is at position 7"
  putStrLn "  - Then comes \\r (byte 7) and \\n (byte 8)"
  putStrLn "  - So bottomOffset should be 9 (first byte after the line's \\r\\n)"
  putStrLn ""
  
  putStrLn $ "But the formula gives: " ++ show bottomOffset
  putStrLn "Expected value: 9 (file size)"
  putStrLn ""
  
  if bottomOffset == 9
    then putStrLn "✓ Formula is CORRECT in this case!"
    else putStrLn $ "✗ Formula gives WRONG value: " ++ show bottomOffset ++ " (expected 9)"
  putStrLn ""
  
  putStrLn "WAIT - the formula includes \"+1\" for the newline!"
  putStrLn "So: 6 + 1 + 1 = 8 (not 9)"
  putStrLn ""
  putStrLn "THE BUG: The formula doesn't account for \\r!"
  putStrLn "File structure: 'C' (1 byte) + '\\r' (1 byte) + '\\n' (1 byte) = 3 bytes"
  putStrLn "Current formula: lastOff + len(C) + 1 = 6 + 1 + 1 = 8"
  putStrLn "But the actual end of the line in the file is byte 9!"
  putStrLn ""
  putStrLn "The formula assumes Unix LF-only line endings!"
  putStrLn "For CRLF files, it needs to add 2 instead of 1."
