{-# LANGUAGE OverloadedStrings #-}
-- Quick test of BidirectionalScanner
import qualified Data.ByteString as BS
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hClose)
import qualified Data.Text.IO as TIO
import HaFileViewer.BidirectionalScanner
import System.IO.MMap (mmapFileByteString)

-- Simple wrapper to read from mmap
readFromFile :: FilePath -> Offset -> Integer -> IO BS.ByteString
readFromFile path offset size = do
  content <- mmapFileByteString path Nothing
  return $ BS.take (fromIntegral size) $ BS.drop (fromIntegral offset) content

main :: IO ()
main = do
  putStrLn "Testing BidirectionalScanner..."
  
  withSystemTempFile "bidir_test.txt" $ \path h -> do
    hPutStr h "line1\nline2\nline3\nline4\nline5\n"
    hClose h
    
    putStrLn "\nForward scan (first 3 lines):"
    fwdLines <- scanLines Forward 30 (readFromFile path) 3
    mapM_ TIO.putStrLn fwdLines
    
    putStrLn "\nBackward scan (last 3 lines):"
    bwdLines <- scanLines Backward 30 (readFromFile path) 3
    mapM_ TIO.putStrLn bwdLines
    
    putStrLn "\nTest complete!"
