{-# LANGUAGE OverloadedStrings #-}
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
  -- Test 1: "a\n\nb\n"
  withSystemTempFile "test.txt" $ \path h -> do
    hPutStr h "a\n\nb\n"
    hClose h
    
    putStrLn "Test: 'a\\n\\nb\\n'"
    result <- scanLines Forward 5 (readFromFile path) 5
    print result
    putStrLn $ "Expected: [\"a\", \"\", \"b\"]"
    putStrLn $ "Match: " ++ show (result == ["a", "", "b"])
