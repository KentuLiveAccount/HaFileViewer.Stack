{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HaFileViewer.LineMap

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "HaFileViewer.LineMap" $ do
  it "returns correct slice and supports negative indexing" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "test.txt"
          contents = T.unlines ["line1","line2","line3","line4","line5"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- read lines 1..3 (0-based start)
      ls <- getLines lm 1 3
      ls `shouldBe` map T.pack ["line2","line3","line4"]
      -- negative start: -2 => last two lines
      ls2 <- getLines lm (-2) 2
      ls2 `shouldBe` map T.pack ["line4","line5"]
      closeLineMap lm

  it "returns empty list for empty file" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "empty.txt"
      TIO.writeFile fp ""
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm 0 10
      ls `shouldBe` ([] :: [T.Text])
      closeLineMap lm

  it "start beyond EOF returns empty list" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "few.txt"
          contents = T.unlines ["a","b","c"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm 100 5
      ls `shouldBe` ([] :: [T.Text])
      closeLineMap lm

  it "count larger than remaining returns remaining lines" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "few2.txt"
          contents = T.intercalate "\n" ["one","two","three"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm 1 10
      ls `shouldBe` map T.pack ["two","three"]
      closeLineMap lm

  it "handles EOF access (last partial line returned)" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "eof.txt"
          contents = T.intercalate "" ["L1\n", "L2\n", "L3"] -- last line without trailing newline
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls1 <- getLines lm 2 1
      ls1 `shouldBe` map T.pack ["L3"]
      ls2 <- getLines lm 3 1
      ls2 `shouldBe` ([] :: [T.Text])
      closeLineMap lm

  it "handles chunk boundary for very long lines" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "longline.txt"
          longLine = T.replicate 70000 "a"
          contents = T.concat [longLine, "\n", "tail"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      l0 <- getLines lm 0 1
      l0 `shouldBe` [longLine]
      l1 <- getLines lm 1 1
      l1 `shouldBe` map T.pack ["tail"]
      closeLineMap lm
