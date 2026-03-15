{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hClose, Handle)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import HaFileViewer.Backend.BidirectionalScanner
import System.IO.MMap (mmapFileByteString)

-- Helper to read from file using mmap
readFromFile :: FilePath -> Integer -> Integer -> IO BS.ByteString
readFromFile path offset size = do
  content <- mmapFileByteString path Nothing
  return $ BS.take (fromIntegral size) $ BS.drop (fromIntegral offset) content

-- Helper to create temp file with content
withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile content action = 
  withSystemTempFile "bidir_test.txt" $ \path h -> do
    hPutStr h content
    hClose h
    action path

-- Helper: scan with auto-detected file size
scanLinesAuto :: Direction -> FilePath -> Int -> IO [T.Text]
scanLinesAuto dir path count = do
  size <- fromIntegral . BS.length <$> BS.readFile path
  scanLines dir size (readFromFile path) count

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "HaFileViewer.BidirectionalScanner" $ do
  
  describe "Forward scanning" $ do
    it "reads lines from start of file" $
      withTempFile "line1\nline2\nline3\n" $ \path -> do
        result <- scanLines Forward 18 (readFromFile path) 2
        result `shouldBe` ["line1", "line2"]
    
    it "handles file with trailing newline" $
      withTempFile "a\nb\nc\n" $ \path -> do
        result <- scanLinesAuto Forward path 3
        result `shouldBe` ["a", "b", "c"]
    
    it "handles file without trailing newline" $
      withTempFile "a\nb\nc" $ \path -> do
        result <- scanLinesAuto Forward path 3
        result `shouldBe` ["a", "b", "c"]
    
    it "handles single line with newline" $
      withTempFile "single\n" $ \path -> do
        result <- scanLines Forward 7 (readFromFile path) 1
        result `shouldBe` ["single"]
    
    it "handles single line without newline" $
      withTempFile "single" $ \path -> do
        result <- scanLines Forward 6 (readFromFile path) 1
        result `shouldBe` ["single"]
    
    it "handles empty file" $
      withTempFile "" $ \path -> do
        result <- scanLines Forward 0 (readFromFile path) 1
        result `shouldBe` ([] :: [T.Text])
    
    it "stops when requested count is reached" $
      withTempFile "1\n2\n3\n4\n5\n" $ \path -> do
        result <- scanLines Forward 10 (readFromFile path) 3
        result `shouldBe` ["1", "2", "3"]
  
  describe "Backward scanning" $ do
    it "reads lines from end of file" $
      withTempFile "line1\nline2\nline3\n" $ \path -> do
        result <- scanLinesAuto Backward path 2
        result `shouldBe` ["line2", "line3"]
    
    it "handles file with trailing newline" $
      withTempFile "a\nb\nc\n" $ \path -> do
        result <- scanLinesAuto Backward path 3
        result `shouldBe` ["a", "b", "c"]
    
    it "handles file without trailing newline" $
      withTempFile "a\nb\nc" $ \path -> do
        result <- scanLinesAuto Backward path 3
        result `shouldBe` ["a", "b", "c"]
    
    it "reads last line from file without trailing newline" $
      withTempFile "a\nb\nc" $ \path -> do
        result <- scanLinesAuto Backward path 1
        result `shouldBe` ["c"]
    
    it "handles single line with newline" $
      withTempFile "single\n" $ \path -> do
        result <- scanLinesAuto Backward path 1
        result `shouldBe` ["single"]
    
    it "handles single line without newline" $
      withTempFile "single" $ \path -> do
        result <- scanLinesAuto Backward path 1
        result `shouldBe` ["single"]
    
    it "handles empty file" $
      withTempFile "" $ \path -> do
        result <- scanLinesAuto Backward path 1
        result `shouldBe` ([] :: [T.Text])
    
    it "stops when requested count is reached" $
      withTempFile "1\n2\n3\n4\n5\n" $ \path -> do
        result <- scanLinesAuto Backward path 3
        result `shouldBe` ["3", "4", "5"]
  
  describe "Symmetry tests" $ do
    it "forward and backward return same lines for whole file (with trailing LF)" $
      withTempFile "line1\nline2\nline3\n" $ \path -> do
        fwd <- scanLinesAuto Forward path 10
        bwd <- scanLinesAuto Backward path 10
        fwd `shouldBe` bwd
    
    it "forward and backward return same lines for whole file (no trailing LF)" $
      withTempFile "line1\nline2\nline3" $ \path -> do
        fwd <- scanLinesAuto Forward path 10
        bwd <- scanLinesAuto Backward path 10
        fwd `shouldBe` bwd
    
    it "forward from start N lines = backward from start N lines" $
      withTempFile "a\nb\nc\nd\ne\nf\n" $ \path -> do
        fwd <- scanLinesAuto Forward path 3
        bwd <- scanLinesAuto Backward path 6
        fwd `shouldBe` take 3 bwd
  
  describe "Edge cases" $ do
    it "handles lines with only newlines" $
      withTempFile "\n\n\n" $ \path -> do
        result <- scanLinesAuto Forward path 5
        result `shouldBe` ["", "", ""]
    
    it "handles empty lines in middle" $
      withTempFile "a\n\nb\n" $ \path -> do
        result <- scanLinesAuto Forward path 5
        result `shouldBe` ["a", "", "b"]
    
    it "handles very long line" $
      let longLine = T.replicate 10000 "x"
          content = T.unpack longLine ++ "\n"
      in withTempFile content $ \path -> do
        result <- scanLinesAuto Forward path 1
        result `shouldBe` [longLine]
    
    it "backward handles empty lines" $
      withTempFile "a\n\nb\n" $ \path -> do
        result <- scanLinesAuto Backward path 5
        result `shouldBe` ["a", "", "b"]
