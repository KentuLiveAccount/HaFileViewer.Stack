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

  -- Negative indexing tests (for bidirectional index optimization)
  it "negative index: last line only" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "negidx1.txt"
          contents = T.unlines ["L1", "L2", "L3", "L4", "L5"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm (-1) 1
      ls `shouldBe` [T.pack "L5"]
      closeLineMap lm

  it "negative index: last 3 lines" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "negidx2.txt"
          contents = T.unlines (map (\n -> "line" <> T.pack (show n)) [1..10::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm (-3) 3
      ls `shouldBe` map T.pack ["line8", "line9", "line10"]
      closeLineMap lm

  it "negative index: all lines via -n" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "negidx3.txt"
          contents = T.unlines ["A", "B", "C"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm (-3) 3
      ls `shouldBe` map T.pack ["A", "B", "C"]
      closeLineMap lm

  it "negative index beyond file start returns partial" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "negidx4.txt"
          contents = T.intercalate "\n" ["X", "Y"]  -- No trailing newline
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm (-5) 10
      ls `shouldBe` map T.pack ["X", "Y"]
      closeLineMap lm

  it "negative index with count exceeding available" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "negidx5.txt"
          contents = T.intercalate "\n" ["P1", "P2", "P3"]  -- No trailing newline
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      ls <- getLines lm (-2) 100
      ls `shouldBe` map T.pack ["P2", "P3"]
      closeLineMap lm

  -- Convergence scenario tests (forward + backward access patterns)
  it "convergence: forward then backward access" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "converge1.txt"
          -- Create 2000 lines without trailing newline (intercalate)
          contents = T.intercalate "\n" (map (\n -> "L" <> T.pack (show n)) [1..2000::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Forward access (builds forward index)
      front <- getLines lm 0 5
      front `shouldBe` map (\n -> T.pack $ "L" ++ show n) [1..5::Int]
      -- Backward access: -5 means 5th from end (line 1996 in 2000-line file)
      back <- getLines lm (-5) 5
      back `shouldBe` map (\n -> T.pack $ "L" ++ show n) [1996..2000::Int]
      -- Middle access (would trigger convergence check)
      mid <- getLines lm 1000 3
      mid `shouldBe` map (\n -> T.pack $ "L" ++ show n) [1001..1003::Int]
      closeLineMap lm

  it "convergence: interleaved forward and backward" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "converge2.txt"
          contents = T.intercalate "\n" (map (\n -> T.pack (show n)) [1..5000::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Alternate between forward and backward access
      l1 <- getLines lm 100 2
      l2 <- getLines lm (-100) 2
      l3 <- getLines lm 2500 2
      l4 <- getLines lm (-50) 2
      l1 `shouldBe` map (T.pack . show) [101, 102::Int]
      l2 `shouldBe` map (T.pack . show) [4901, 4902::Int]
      l3 `shouldBe` map (T.pack . show) [2501, 2502::Int]
      l4 `shouldBe` map (T.pack . show) [4951, 4952::Int]
      closeLineMap lm

  it "convergence: same region via positive and negative" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "converge3.txt"
          contents = T.unlines (map (\n -> "N" <> T.pack (show n)) [0..999::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Access line 500 from both directions
      fromStart <- getLines lm 500 1
      fromEnd <- getLines lm (-500) 1
      -- Both should give same result
      fromStart `shouldBe` [T.pack "N500"]
      fromEnd `shouldBe` [T.pack "N500"]
      closeLineMap lm

  it "large file with sparse access pattern" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "sparse.txt"
          -- 10000 lines numbered LINE1 through LINE10000 (no trailing newline)
          contents = T.intercalate "\n" (map (\n -> "LINE" <> T.pack (show n)) [1..10000::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Jump around to build sparse index (remember: 0-based indexing)
      r1 <- getLines lm 0 1      -- index 0 = LINE1
      r1 `shouldBe` [T.pack "LINE1"]
      -- Test positive index first
      r4 <- getLines lm 2500 1   -- index 2500 = LINE2501
      r4 `shouldBe` [T.pack "LINE2501"]
      -- Now test last line - this requires countTotalLines
      allLns <- getLines lm 0 20000  -- Get all lines
      let actualTotal = length allLns
          lastLine = if null allLns then T.pack "EMPTY" else last allLns
      actualTotal `shouldBe` 10000
      -- Now try negative indexing
      r3 <- getLines lm (-1) 1   -- last line = LINE10000
      r3 `shouldBe` [T.pack "LINE10000"]
      closeLineMap lm
