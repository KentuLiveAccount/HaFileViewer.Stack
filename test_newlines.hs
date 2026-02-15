{-# LANGUAGE OverloadedStrings #-}
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
  withSystemTempFile "test_newlines.txt" $ \path h -> do
    let content = "\n\n\n"
    hPutStr h content
    hClose h
    
    -- Get actual file size
    actualSize <- fromIntegral . BS.length <$> BS.readFile path
    putStrLn $ "Content: " ++ show content
    putStrLn $ "Size: " ++ show actualSize
    
    -- Test forward scan
    result <- scanLines Forward actualSize (readFromFile path) 5
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: [\"\", \"\", \"\"]"
    putStrLn $ if result == ["", "", ""] then "PASS" else "FAIL"
