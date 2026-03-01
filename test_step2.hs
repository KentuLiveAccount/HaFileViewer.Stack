{-# LANGUAGE OverloadedStrings #-}
-- Step 2: Unit tests for calculatePieceOffsets

import qualified Data.ByteString as BS

type Offset = Integer

-- Copy calculatePieceOffsets here for testing
calculatePieceOffsets :: Offset -> [BS.ByteString] -> [Offset]
calculatePieceOffsets _startOffset [] = []
calculatePieceOffsets startOffset pieces =
  let go _ [] = []
      go currentOffset (piece:rest) =
        let nextOffset = currentOffset + fromIntegral (BS.length piece) + 1  -- +1 for LF
        in currentOffset : go nextOffset rest
  in go startOffset pieces

main :: IO ()
main = do
  putStrLn "Step 2: Testing calculatePieceOffsets"
  
  -- Test 1: Single piece
  let test1 = calculatePieceOffsets 0 ["hello"]
  putStrLn $ "Test 1 (single piece): " ++ show test1
  if test1 == [0] 
    then putStrLn "  PASS"
    else putStrLn $ "  FAIL: expected [0], got " ++ show test1
  
  -- Test 2: Two pieces: "hello\nworld" splits to ["hello", "world", ""]
  let test2 = calculatePieceOffsets 0 ["hello", "world"]
  putStrLn $ "Test 2 (two pieces): " ++ show test2
  if test2 == [0, 6] 
    then putStrLn "  PASS"
    else putStrLn $ "  FAIL: expected [0, 6], got " ++ show test2
  
  -- Test 3: Three 1-byte pieces
  let test3 = calculatePieceOffsets 0 ["a", "b", "c"]
  putStrLn $ "Test 3 (three 1-byte pieces): " ++ show test3
  if test3 == [0, 2, 4]
    then putStrLn "  PASS"
    else putStrLn $ "  FAIL: expected [0, 2, 4], got " ++ show test3
  
  -- Test 4: Offset chaining with start position
  let test4 = calculatePieceOffsets 100 ["abc", "de"]
  putStrLn $ "Test 4 (offset chaining): " ++ show test4
  if test4 == [100, 104]
    then putStrLn "  PASS"
    else putStrLn $ "  FAIL: expected [100, 104], got " ++ show test4
  
  -- Test 5: Empty pieces handling
  -- Empty piece has length 0, so offset advances by 0 + 1 (for LF) = 1
  let test5 = calculatePieceOffsets 0 ["", "a", ""]
  putStrLn $ "Test 5 (empty pieces): " ++ show test5
  if test5 == [0, 1, 3]  -- 0, then 0+1=1, then 1+1+1=3
    then putStrLn "  PASS"
    else putStrLn $ "  FAIL: expected [0, 1, 3], got " ++ show test5
  
  -- Test 6: Verify UTF-8 encoding (just use ASCII for simplicity)
  let test6 = calculatePieceOffsets 0 ["ab", "c"]
  putStrLn $ "Test 6 (multi-char): " ++ show test6
  if test6 == [0, 3]  -- 2 bytes for "ab" + 1 for LF = 3
    then putStrLn "  PASS"
    else putStrLn $ "  FAIL: expected [0, 3], got " ++ show test6
  
  putStrLn "\nAll tests completed!"
