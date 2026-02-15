{-# LANGUAGE OverloadedStrings #-}
-- Debug test for "reads lines from end" failure

import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.IO
import System.IO.Temp (withSystemTempFile)
import System.IO.MMap (mmapFileByteString)
import HaFileViewer.BidirectionalScanner

readFromFile :: FilePath -> Integer -> Integer -> IO BS.ByteString
readFromFile path offset size = do
  content <- mmapFileByteString path Nothing
  return $ BS.take (fromIntegral size) $ BS.drop (fromIntegral offset) content

main :: IO ()
main = do
  withSystemTempFile "test_debug.txt" $ \path h -> do
    let content = "line1\nline2\nline3\n"
    hPutStr h content
    hClose h
    
    -- Get actual file size
    actualSize <- fromIntegral . BS.length <$> BS.readFile path
    putStrLn $ "Content written: " ++ show content
    putStrLn $ "Expected size: 18, Actual size: " ++ show actualSize
    
    -- Test backward scan
    result <- scanLines Backward actualSize (readFromFile path) 2
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: [\"line2\", \"line3\"]"
    putStrLn $ if result == ["line2", "line3"] then "PASS" else "FAIL"
