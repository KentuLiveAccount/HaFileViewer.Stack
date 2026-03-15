{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import Data.IORef
import Control.Monad (forM_)

-- Simple scanner test without using the actual module
-- This is to debug the offset calculation logic

main :: IO ()
main = do
  putStrLn "=== Scanner Offset Calculation Test ==="
  putStrLn ""
  
  -- Simulate what calculatePieceOffsets does
  let testContent = "A\r\nB\r\nC\r\n"
      pieces = ["A", "B", "C", ""]  -- What BS.split on LF would give
      
  putStrLn "Input pieces (after split on LF):"
  forM_ (zip [0..] pieces) $ \(idx, p) -> 
    putStrLn $ "  [" ++ show idx ++ "]: \"" ++ p ++ "\" (length " ++ show (length p) ++ ")"
  putStrLn ""
  
  -- Manually calculate offsets using the formula from calculatePieceOffsets
  let calculateOffsets startOffset ps = go startOffset ps
        where
          go _ [] = []
          go currentOffset (p:rest) =
            let nextOffset = currentOffset + length p + 1  -- +1 for LF
            in currentOffset : go nextOffset rest
  
  let offsets = calculateOffsets 0 pieces
  putStrLn "Calculated offsets (formula: offset[i] = sum of (len(p[0..i-1]) + 1) for each p):"
  forM_ (zip [0..] offsets) $ \(idx, o) -> 
    putStrLn $ "  [" ++ show idx ++ "]: " ++ show o
  putStrLn ""
  
  putStrLn "Expected offsets (from file structure):"
  putStrLn "  [0] (\"A\"): 0"
  putStrLn "  [1] (\"B\"): 3 (0 + 1 (\"A\") + 1 (\\r) + 1 (\\n) = 3)"
  putStrLn "  [2] (\"C\"): 6 (3 + 1 (\"B\") + 1 (\\r) + 1 (\\n) = 6)"  
  putStrLn "  [3] (\"\"): 9 (6 + 1 (\"C\") + 1 (\\r) + 1 (\\n) = 9)"
  putStrLn ""
  
  -- But wait - BS.split excludes the delimiter (LF) but includes CR!
  -- So the pieces from BS.split on LF of "A\r\nB\r\nC\r\n" are:
  let actualPieces = ["A\r", "B\r", "C\r", ""]  -- CR is included!
      actualOffsets = calculateOffsets 0 actualPieces
  
  putStrLn "ACTUAL pieces (with CR, since BS.split keeps everything except LF):"
  forM_ (zip [0..] actualPieces) $ \(idx, p) -> 
    putStrLn $ "  [" ++ show idx ++ "]: " ++ show p ++ " (length " ++ show (length p) ++ ")"
  putStrLn ""
  
  putStrLn "ACTUAL calculated offsets:"
  forM_ (zip [0..] actualOffsets) $ \(idx, o) -> 
    putStrLn $ "  [" ++ show idx ++ "]: " ++ show o
  putStrLn ""
  
  -- Now check: what about stripCR?
  let stripCR s = if null s || last s /= '\r' then s else init s
      strippedPieces = map stripCR actualPieces
      
  putStrLn "After stripCR:"
  forM_ (zip [0..] strippedPieces) $ \(idx, p) -> 
    putStrLn $ "  [" ++ show idx ++ "]: " ++ show p ++ " (length " ++ show (length p) ++ ")"
  putStrLn ""
  
  putStrLn "KEY FINDING:"
  putStrLn "===================="
  putStrLn ""
  putStrLn "When BS.split is used on CRLF-terminated lines:"
  putStrLn "  - Input: \"A\\r\\nB\\r\\nC\\r\\n\""
  putStrLn "  - Pieces with CR: [\"A\\r\", \"B\\r\", \"C\\r\", \"\"]"
  putStrLn "  - Calculated offsets: [0, 3, 6, 9]"
  putStrLn ""
  putStrLn "After stripCR:"
  putStrLn "  - Pieces: [\"A\", \"B\", \"C\", \"\"]"
  putStrLn "  - Offsets UNCHANGED: [0, 3, 6, 9]"
  putStrLn ""
  putStrLn "This means:"
  putStrLn "  - stripCR is applied to pieces AFTER offset calculation ✓"
  putStrLn "  - Offset 3 points to 'B' in the file ✓"
  putStrLn "  - BUT: offset includes the \\r if present in original!"
  putStrLn ""
  putStrLn "The scanner IS calculating offsets correctly!"
  putStrLn "The issue might be in LineCache's bottomOffset formula."
