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

  it "negative access with partial forward index" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "partial.txt"
          -- Create 3000 lines (spans ~3 index steps)
          contents = T.intercalate "\n" (map (\n -> "LINE" <> T.pack (show n)) [1..3000::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Build partial forward index (only first ~500 lines)
      front <- getLines lm 0 500
      length front `shouldBe` 500
      case (front, reverse front) of
        (f:_, l:_) -> do
          f `shouldBe` T.pack "LINE1"
          l `shouldBe` T.pack "LINE500"
        _ -> expectationFailure "Expected non-empty list"
      -- Now access from end - forward index is incomplete
      -- This should trigger countTotalLines which scans the rest of the file
      back <- getLines lm (-10) 10
      back `shouldBe` map (\n -> T.pack $ "LINE" ++ show n) [2991..3000::Int]
      -- Verify we can still access middle (should use partial index)
      mid <- getLines lm 1500 5
      mid `shouldBe` map (\n -> T.pack $ "LINE" ++ show n) [1501..1505::Int]
      closeLineMap lm

  it "negative access with fully-built forward index" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "fullindex.txt"
          -- Create 2500 lines
          contents = T.intercalate "\n" (map (\n -> "N" <> T.pack (show n)) [1..2500::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Build complete forward index by reading entire file
      allLines <- getLines lm 0 3000  -- Request more than exists
      length allLines `shouldBe` 2500
      case (allLines, reverse allLines) of
        (f:_, l:_) -> do
          f `shouldBe` T.pack "N1"
          l `shouldBe` T.pack "N2500"
        _ -> expectationFailure "Expected non-empty list"
      -- Now negative access - forward index is complete
      -- countTotalLines should still work correctly
      lastTen <- getLines lm (-10) 10
      lastTen `shouldBe` map (\n -> T.pack $ "N" ++ show n) [2491..2500::Int]
      -- Access from middle using negative index
      fromEnd500 <- getLines lm (-500) 5
      fromEnd500 `shouldBe` map (\n -> T.pack $ "N" ++ show n) [2001..2005::Int]
      closeLineMap lm

  -- Future: Forward access when backward table exists (for bidirectional index)
  it "forward access when backward table would overlap" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "fwd_back_overlap.txt"
          -- Create 3000 lines
          contents = T.intercalate "\n" (map (\n -> "R" <> T.pack (show n)) [1..3000::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Simulate: access from end first (would build backward table)
      end <- getLines lm (-50) 10
      end `shouldBe` map (\n -> T.pack $ "R" ++ show n) [2951..2960::Int]
      -- Now access from middle-to-end range (overlaps with backward)
      -- Should use forward scan, not be confused by backward table
      midToEnd <- getLines lm 2900 50
      midToEnd `shouldBe` map (\n -> T.pack $ "R" ++ show n) [2901..2950::Int]
      -- Access near end again
      nearEnd <- getLines lm 2980 10
      nearEnd `shouldBe` map (\n -> T.pack $ "R" ++ show n) [2981..2990::Int]
      closeLineMap lm

  it "forward access in range not covered by backward table" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "fwd_no_back.txt"
          -- Create 4000 lines spanning multiple index steps
          contents = T.intercalate "\n" (map (\n -> "D" <> T.pack (show n)) [1..4000::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Access last 100 lines (would build backward table in that region)
      lastHundred <- getLines lm (-100) 50
      lastHundred `shouldBe` map (\n -> T.pack $ "D" ++ show n) [3901..3950::Int]
      -- Now access beginning/middle (outside backward table coverage)
      -- Should use forward table, unaffected by backward
      beginning <- getLines lm 0 10
      beginning `shouldBe` map (\n -> T.pack $ "D" ++ show n) [1..10::Int]
      middle <- getLines lm 2000 20
      middle `shouldBe` map (\n -> T.pack $ "D" ++ show n) [2001..2020::Int]
      -- Access in gap between forward and backward
      gap <- getLines lm 3500 10
      gap `shouldBe` map (\n -> T.pack $ "D" ++ show n) [3501..3510::Int]
      closeLineMap lm

  it "forward scan through potential convergence point" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "convergence_point.txt"
          -- Create 2048 lines (exactly 2 * indexStepDefault)
          contents = T.intercalate "\n" (map (\n -> "C" <> T.pack (show n)) [1..2048::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Build forward up to line 1024
      fwd1 <- getLines lm 1000 10
      fwd1 `shouldBe` map (\n -> T.pack $ "C" ++ show n) [1001..1010::Int]
      -- Access from end (would build backward to ~line 1024)
      back1 <- getLines lm (-1024) 10
      back1 `shouldBe` map (\n -> T.pack $ "C" ++ show n) [1025..1034::Int]
      -- Access exactly at convergence point (line 1024)
      -- Should work correctly whether using forward or backward table
      atConvergence <- getLines lm 1024 1
      atConvergence `shouldBe` [T.pack "C1025"]
      -- Scan across convergence point
      acrossConvergence <- getLines lm 1020 10
      acrossConvergence `shouldBe` map (\n -> T.pack $ "C" ++ show n) [1021..1030::Int]
      closeLineMap lm

  -- Forward access tests (for future backward index validation)
  it "forward access with simulated backward table overlap" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "fwdbackoverlap.txt"
          -- 4000 lines to span multiple index steps
          contents = T.intercalate "\n" (map (\n -> "R" <> T.pack (show n)) [1..4000::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Simulate backward table: access from end first
      backAccess <- getLines lm (-100) 50
      length backAccess `shouldBe` 50
      -- Now forward access in middle (would overlap if backward index exists)
      midAccess <- getLines lm 2000 10
      midAccess `shouldBe` map (\n -> T.pack $ "R" ++ show n) [2001..2010::Int]
      -- Forward access near start (no overlap with backward)
      startAccess <- getLines lm 100 5
      startAccess `shouldBe` map (\n -> T.pack $ "R" ++ show n) [101..105::Int]
      closeLineMap lm

  it "forward access when backward table exists but no overlap" $
    withSystemTempDirectory "hfvt" $ \dir -> do
      let fp = dir </> "fwdbackseparate.txt"
          -- 5000 lines
          contents = T.intercalate "\n" (map (\n -> "S" <> T.pack (show n)) [1..5000::Int])
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Build backward table by accessing end
      end1 <- getLines lm (-50) 25
      length end1 `shouldBe` 25
      end2 <- getLines lm (-500) 50
      length end2 `shouldBe` 50
      -- Forward access at start (no overlap - tests isolation)
      start <- getLines lm 0 10
      start `shouldBe` map (\n -> T.pack $ "S" ++ show n) [1..10::Int]
      -- Forward access in early middle (still no overlap)
      earlyMid <- getLines lm 1000 5
      earlyMid `shouldBe` map (\n -> T.pack $ "S" ++ show n) [1001..1005::Int]
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
      actualTotal `shouldBe` 10000
      -- Now try negative indexing
      r3 <- getLines lm (-1) 1   -- last line = LINE10000
      r3 `shouldBe` [T.pack "LINE10000"]
      closeLineMap lm
