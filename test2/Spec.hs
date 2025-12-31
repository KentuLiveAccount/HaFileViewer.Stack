{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HaFileViewer.LineMap2

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "HaFileViewer.LineMap2 (Unified Index)" $ do
  it "basic structure compiles and opens file" $
    withSystemTempDirectory "hfvt2" $ \dir -> do
      let fp = dir </> "test.txt"
          contents = T.unlines ["line1","line2","line3"]
      TIO.writeFile fp contents
      lm <- openLineMap fp indexStepDefault
      -- Currently returns empty, but structure compiles
      result <- getLines lm 0 3
      result `shouldBe` []
      closeLineMap lm
