{-# LANGUAGE OverloadedStrings #-}

-- Debug: What does getLinesFrom return for Forward vs Backward?

import HaFileViewer.LineCache
import HaFileViewer.BidirectionalScanner (Direction(..))
import Control.Monad (forM_)
import qualified Data.Text as T

testFile :: FilePath
testFile = "test-onenote.log"

main :: IO ()
main = do
  putStrLn "=== Testing getLinesFrom Forward vs Backward ==="
  putStrLn ""
  
  cache <- openLineCache testFile
  
  -- Get initial lines (1-30)
  putStrLn "1. Initial read: getLinesFromStart 30"
  (initial, topPos1, botPos1) <- getLinesFromStart cache 30
  putStrLn $ "   Got " ++ show (length initial) ++ " lines"
  putStrLn $ "   Lines 25-30:"
  forM_ (drop 24 (take 30 initial)) $ \(text, lineNum) -> do
    let preview = T.take 60 text
    putStrLn $ "     Line " ++ show lineNum ++ ": len=" ++ show (T.length text) ++ " [" ++ T.unpack preview ++ "]"
  
  putStrLn ""
  putStrLn "2. Forward read from bottom position (should get line 31)"
  (fwdLines, topPos2, botPos2) <- getLinesFrom cache botPos1 Forward 1 31
  putStrLn $ "   Got " ++ show (length fwdLines) ++ " lines"
  forM_ fwdLines $ \(text, lineNum) -> do
    putStrLn $ "     Line " ++ show lineNum ++ ": len=" ++ show (T.length text) 
            ++ " empty=" ++ show (T.null text)
            ++ if T.null text then " <<<EMPTY>>>" else " [" ++ T.unpack (T.take 60 text) ++ "]"
  
  putStrLn ""
  putStrLn "3. Forward read again (should get line 32)"
  (fwdLines2, topPos3, botPos3) <- getLinesFrom cache botPos2 Forward 1 32
  putStrLn $ "   Got " ++ show (length fwdLines2) ++ " lines"
  forM_ fwdLines2 $ \(text, lineNum) -> do
    putStrLn $ "     Line " ++ show lineNum ++ ": len=" ++ show (T.length text) 
            ++ " empty=" ++ show (T.null text)
            ++ if T.null text then " <<<EMPTY>>>" else " [" ++ T.unpack (T.take 60 text) ++ "]"
  
  putStrLn ""
  putStrLn "4. Backward read from top position (should get line 29)"
  (bwdLines, topPos4, botPos4) <- getLinesFrom cache topPos1 Backward 1 29
  putStrLn $ "   Got " ++ show (length bwdLines) ++ " lines"
  forM_ bwdLines $ \(text, lineNum) -> do
    putStrLn $ "     Line " ++ show lineNum ++ ": len=" ++ show (T.length text) 
            ++ " empty=" ++ show (T.null text)
            ++ if T.null text then " <<<EMPTY>>>" else " [" ++ T.unpack (T.take 60 text) ++ "]"
  
  putStrLn ""
  putStrLn "5. Backward read again (should get line 28)"
  (bwdLines2, topPos5, botPos5) <- getLinesFrom cache topPos4 Backward 1 28
  putStrLn $ "   Got " ++ show (length bwdLines2) ++ " lines"
  forM_ bwdLines2 $ \(text, lineNum) -> do
    putStrLn $ "     Line " ++ show lineNum ++ ": len=" ++ show (T.length text) 
            ++ " empty=" ++ show (T.null text)
            ++ if T.null text then " <<<EMPTY>>>" else " [" ++ T.unpack (T.take 60 text) ++ "]"
  
  closeLineCache cache
  putStrLn ""
  putStrLn "=== Test Complete ==="
