{-# LANGUAGE OverloadedStrings #-}
-- Simple backward test
import qualified Data.ByteString as BS
import qualified Data.Text as T
import HaFileViewer.BidirectionalScanner
import System.IO.Temp
import System.IO

readFromFile :: FilePath -> Integer -> Integer -> IO BS.ByteString
readFromFile path offset size = do
  content <- BS.readFile path
  return $ BS.take (fromIntegral size) $ BS.drop (fromIntegral offset) content

main :: IO ()
main = do
  putStrLn "=== Backward scan test ==="
  withSystemTempFile "test.txt" $ \path h -> do
    hPutStr h "a\nb\nc\n"
    hClose h
    
    content <- BS.readFile path
    let size = fromIntegral $ BS.length content
    putStrLn $ "File content bytes: " ++ show (BS.unpack content)
    putStrLn $ "File size: " ++ show size
    
    result <- scanLines Backward size (readFromFile path) 2
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: [\"b\", \"c\"]"
    putStrLn $ "Match: " ++ show (result == ["b", "c"])
