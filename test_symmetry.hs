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
  withSystemTempFile "test_sym.txt" $ \path h -> do
    let content = "a\nb\nc\nd\ne\nf\n"
    hPutStr h content
    hClose h
    
    actualSize <- fromIntegral . BS.length <$> BS.readFile path
    putStrLn $ "File size: " ++ show actualSize
    
    fwd <- scanLines Forward actualSize (readFromFile path) 3
    putStrLn $ "Forward 3: " ++ show fwd
    
    bwd <- scanLines Backward actualSize (readFromFile path) 6
    putStrLn $ "Backward 6: " ++ show bwd
    
    putStrLn $ "take 3 bwd: " ++ show (take 3 bwd)
    putStrLn $ "Expected: [\"a\", \"b\", \"c\"]"
