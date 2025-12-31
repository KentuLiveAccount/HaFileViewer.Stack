{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HaFileViewer.LineMap2
import Data.Time.Clock (getCurrentTime, diffUTCTime)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "HaFileViewer.LineMap2 (Unified Index)" $ do
  it "returns correct slice and supports negative indexing" $
    withSystemTempDirectory "hfvt2" $ \dir -> do
      let fp = dir </> "test.txt"
          contents = T.unlines ["line1","line2","line3","line4","line5"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm 1 3
      ls `shouldBe` ["line2","line3","line4"]
      ls2 <- getLines lm (-2) 2
      ls2 `shouldBe` ["line4","line5"]
      closeLineMap lm

  it "returns empty list for empty file" $
    withSystemTempDirectory "hfvt2" $ \dir -> do
      let fp = dir </> "empty.txt"
      TIO.writeFile fp ""
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm 0 10
      ls `shouldBe` ([] :: [T.Text])
      closeLineMap lm

  it "start beyond EOF returns empty list" $
    withSystemTempDirectory "hfvt2" $ \dir -> do
      let fp = dir </> "few.txt"
          contents = T.unlines ["a","b","c"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm 100 5
      ls `shouldBe` ([] :: [T.Text])
      closeLineMap lm

  it "count larger than remaining returns remaining lines" $
    withSystemTempDirectory "hfvt2" $ \dir -> do
      let fp = dir </> "few2.txt"
          contents = T.intercalate "\n" ["one","two","three"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm 1 10
      ls `shouldBe` ["two","three"]
      closeLineMap lm

  it "handles EOF access (last partial line returned)" $
    withSystemTempDirectory "hfvt2" $ \dir -> do
      let fp = dir </> "eof.txt"
          contents = T.intercalate "" ["L1\n", "L2\n", "L3"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls1 <- getLines lm 2 1
      ls1 `shouldBe` ["L3"]
      ls2 <- getLines lm 3 1
      ls2 `shouldBe` ([] :: [T.Text])
      closeLineMap lm

  it "negative index: last line only" $
    withSystemTempDirectory "hfvt2" $ \dir -> do
      let fp = dir </> "negidx1.txt"
          contents = T.unlines ["L1", "L2", "L3", "L4", "L5"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm (-1) 1
      ls `shouldBe` ["L5"]
      closeLineMap lm

  it "negative index: last 3 lines" $
    withSystemTempDirectory "hfvt2" $ \dir -> do
      let fp = dir </> "negidx2.txt"
          contents = T.unlines (map (\n -> "line" <> T.pack (show n)) [1..10::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm (-3) 3
      ls `shouldBe` ["line8", "line9", "line10"]
      closeLineMap lm

  it "negative index: all lines via -n" $
    withSystemTempDirectory "hfvt2" $ \dir -> do
      let fp = dir </> "negidx3.txt"
          contents = T.unlines ["A", "B", "C"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm (-3) 3
      ls `shouldBe` ["A", "B", "C"]
      closeLineMap lm

  it "convergence: forward then backward access" $
    withSystemTempDirectory "hfvt2" $ \dir -> do
      let fp = dir </> "conv1.txt"
          contents = T.intercalate "\n" (map (\n -> "C" <> T.pack (show n)) [1..2500::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Forward access builds forward index
      front <- getLines lm 0 100
      length front `shouldBe` 100
      -- Backward access builds backward index and detects convergence
      back <- getLines lm (-100) 100
      length back `shouldBe` 100
      -- Further backward access should use cached total
      back2 <- getLines lm (-50) 10
      length back2 `shouldBe` 10
      closeLineMap lm

  it "fast tail access on large file without forward index" $
    withSystemTempDirectory "hfvt2" $ \dir -> do
      let fp = dir </> "large20k.txt"
          contents = T.intercalate "\n" (map (\n -> "LINE" <> T.pack (show n)) [1..20000::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      
      -- Time the access
      start <- getCurrentTime
      ls <- getLines lm (-1) 1
      end <- getCurrentTime
      let diff = diffUTCTime end start
      putStrLn $ "\n  getLines(-1) 1 on 20K line file took: " ++ show (floor diff :: Integer) ++ "s"
      
      ls `shouldBe` ["LINE20000"]
      
      -- Also test getting last 5
      ls5 <- getLines lm (-5) 5
      ls5 `shouldBe` ["LINE19996","LINE19997","LINE19998","LINE19999","LINE20000"]
      
      closeLineMap lm
