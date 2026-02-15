{-# LANGUAGE OverloadedStrings #-}
-- Test the actual scanLines function with debug output
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
  putStrLn "=== Test 1: Forward scan 'a\\nb\\nc\\n' expect 3 lines ==="
  withSystemTempFile "test.txt" $ \path h -> do
    hPutStr h "a\nb\nc\n"
    hClose h
    
    fileSize <- BS.length <$> BS.readFile path
    putStrLn $ "File size: " ++ show fileSize
    
    content <- BS.readFile path
    putStrLn $ "Content bytes: " ++ show (BS.unpack content)
    
    result <- scanLines Forward (fromIntegral fileSize) (readFromFile path) 3
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: [\"a\", \"b\", \"c\"]"
    putStrLn $ "Match: " ++ show (result == ["a", "b", "c"])
  
  putStrLn "\n=== Test 2: Forward scan 'a\\nb\\nc\\n' request only 2 lines ==="
  withSystemTempFile "test.txt" $ \path h -> do
    hPutStr h "a\nb\nc\n"
    hClose h
    
    fileSize <- BS.length <$> BS.readFile path
    result <- scanLines Forward (fromIntegral fileSize) (readFromFile path) 2
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: [\"a\", \"b\"]"
    putStrLn $ "Match: " ++ show (result == ["a", "b"])
