{-# LANGUAGE OverloadedStrings #-}
-- Regression Tests for CR-LF Line Ending Bugs
module Main (main) where

import Test.Hspec
import System.IO.Temp (withSystemTempFile)
import System.IO
import qualified Data.Text as T
import qualified Data.ByteString as BS
import HaFileViewer.Backend.LineCache
import HaFileViewer.Backend.BidirectionalScanner (Direction(..))
import Control.Monad (forM_)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "CR-LF Regression Tests" $ do
  
  describe "Simple CR-LF offset validation (test_crlf_offsets.hs)" $ do
    it "correctly reads 3-line CR-LF file" $
      withCRLFFile testLines $ \path -> do
        cache <- openLineCache path
        (lines1, _, _) <- getLinesFromStart cache 3
        
        length lines1 `shouldBe` 3
        let texts = map fst lines1
        texts `shouldBe` map T.pack testLines
        
        closeLineCache cache
  
  describe "Unix LF-only support (test_lf_only.hs)" $ do
    it "correctly handles LF-only files" $
      withLFFile ["Line 1", "Line 2", "Line 3"] $ \path -> do
        cache <- openLineCache path
        (lines1, _, _) <- getLinesFromStart cache 3
        
        length lines1 `shouldBe` 3
        let texts = map fst lines1
        texts `shouldBe` ["Line 1", "Line 2", "Line 3"]
        
        closeLineCache cache
  
  describe "Incremental scroll (test_incremental_scroll.hs)" $ do
    it "reads 50-line CR-LF file incrementally without empty lines" $
      with50LineCRLFFile $ \path -> do
        cache <- openLineCache path
        
        (initial, _, botPos1) <- getLinesFromStart cache 25
        length initial `shouldBe` 25
        
        let emptyInInitial = filter (\(txt, _) -> T.null txt) initial
        emptyInInitial `shouldBe` []
        
        results <- readLinesOneByOne cache botPos1 26 50
        length results `shouldBe` 25
        
        let emptyLines = filter (\(txt, _) -> T.null txt) results
        emptyLines `shouldBe` []
        
        closeLineCache cache
  
  describe "Scrolling from end (Bug #5)" $ do
    it "should be able to read lines before the end" $
      with50LineCRLFFile $ \path -> do
        cache <- openLineCache path
        
        -- Jump to end, get last 25 lines (26-50)
        (linesEnd, _, _) <- getLinesFromEnd cache 25
        length linesEnd `shouldBe` 25
        let lastLineText = fst (last linesEnd)
        lastLineText `shouldBe` "Line 50 has content"
        let firstLineText = fst (head linesEnd)
        firstLineText `shouldBe` "Line 26 has content"
        
        closeLineCache cache
  
  describe "Scroll reversibility (Bug #6)" $ do
    it "should be able to read consecutive ranges incrementally" $
      with50LineCRLFFile $ \path -> do
        cache <- openLineCache path
        
        -- Read lines 1-10
        (lines1, _, bot1) <- getLinesFromStart cache 10
        let firstLine1 = fst (head lines1)
        firstLine1 `shouldBe` "Line 1 has content"
        let lastLine1 = fst (last lines1)
        lastLine1 `shouldBe` "Line 10 has content"
        
        -- Read lines 11-20 from bottom position
        (lines2, _, _) <- getLinesFrom cache bot1 Forward 10 11
        let firstLine2 = fst (head lines2)
        firstLine2 `shouldBe` "Line 11 has content"
        let lastLine2 = fst (last lines2)
        lastLine2 `shouldBe` "Line 20 has content"
        
        closeLineCache cache

testLines :: [String]
testLines = ["Line 1 content", "Line 2 longer text", "Line 3 short"]

withCRLFFile :: [String] -> (FilePath -> IO a) -> IO a
withCRLFFile lines action =
  withSystemTempFile "test-crlf.txt" $ \path h -> do
    hClose h
    withFile path WriteMode $ \h2 -> do
      hSetBinaryMode h2 True
      forM_ lines $ \line ->
        BS.hPutStr h2 (BS.pack $ map (fromIntegral . fromEnum) (line ++ "\r\n"))
    action path

withLFFile :: [String] -> (FilePath -> IO a) -> IO a
withLFFile lines action =
  withSystemTempFile "test-lf.txt" $ \path h -> do
    hClose h
    withFile path WriteMode $ \h2 -> do
      hSetBinaryMode h2 True
      forM_ lines $ \line ->
        BS.hPutStr h2 (BS.pack $ map (fromIntegral . fromEnum) (line ++ "\n"))
    action path

with50LineCRLFFile :: (FilePath -> IO a) -> IO a
with50LineCRLFFile action =
  withSystemTempFile "test-50lines.txt" $ \path h -> do
    hClose h
    withFile path WriteMode $ \h2 -> do
      hSetBinaryMode h2 True
      forM_ [1..50 :: Int] $ \i -> do
        let line = "Line " ++ show i ++ " has content"
        hPutStr h2 (line ++ "\r\n")
    action path

readLinesOneByOne :: LineCache -> LinePosition -> Integer -> Integer -> IO [(T.Text, Integer)]
readLinesOneByOne cache startPos startLine endLine 
  | startLine > endLine = return []
  | otherwise = do
      (lines1, _, botPos) <- getLinesFrom cache startPos Forward 1 startLine
      if null lines1
        then return []
        else do
          rest <- readLinesOneByOne cache botPos (startLine + 1) endLine
          return (lines1 ++ rest)
