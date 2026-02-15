{-# LANGUAGE OverloadedStrings #-}
-- Unit tests for internal BidirectionalScanner functions
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
  putStrLn "=== Testing stripCR ==="
  testStripCR
  
  putStrLn "\n=== Testing BS.split behavior ==="
  testSplit
  
  putStrLn "\n=== Testing canonical format assumption ==="
  testCanonical
  
  putStrLn "\n=== Testing Forward strategy functions ==="
  testForwardStrategyFunctions
  
  putStrLn "\n=== Testing Forward extraction (full) ==="
  testForwardExtraction
  
  testBackwardExtraction

-- Test CR stripping
testStripCR :: IO ()
testStripCR = do
  let stripCR bs
        | BS.null bs = bs
        | BS.last bs == 13 = BS.init bs
        | otherwise = bs
  
  putStrLn $ "stripCR \"abc\\r\" = " ++ show (stripCR "abc\r")
  putStrLn $ "stripCR \"abc\" = " ++ show (stripCR "abc")
  putStrLn $ "stripCR \"\" = " ++ show (stripCR "")

-- Test split behavior with various inputs
testSplit :: IO ()
testSplit = do
  let lfByte = 10
      test input = do
        putStrLn $ "\nInput: " ++ show input
        let pieces = BS.split lfByte input
        putStrLn $ "  Split result: " ++ show pieces
        putStrLn $ "  Piece count: " ++ show (length pieces)
  
  test "a\nb\nc\n"      -- Canonical: ends with LF
  test "a\nb\nc"        -- No trailing LF
  test "\n\n\n"         -- Only newlines
  test "a\n\nb\n"       -- Empty line in middle
  test ""               -- Empty file

-- Test canonical format assumptions
testCanonical :: IO ()
testCanonical = do
  let lfByte = 10
      addLF bs = if BS.null bs || BS.last bs /= lfByte
                 then BS.snoc bs lfByte
                 else bs
  
  putStrLn "\nBefore canonicalization:"
  putStrLn $ "  \"a\\nb\\nc\" -> " ++ show (BS.split lfByte "a\nb\nc")
  
  putStrLn "\nAfter canonicalization:"
  let canonical = addLF "a\nb\nc"
  putStrLn $ "  \"a\\nb\\nc\\n\" -> " ++ show (BS.split lfByte canonical)
  putStrLn $ "  Last piece is empty? " ++ show (last (BS.split lfByte canonical) == "")

-- Test backward extraction logic manually
testBackwardExtraction :: IO ()
testBackwardExtraction = do
  putStrLn "\n=== Testing Backward extraction ==="
  putStrLn "\nFrom \"a\\n\\nb\\n\" (canonical)"
  let pieces = ["a", "", "b", ""]
      partial = ""
  
  putStrLn $ "Pieces (file order): " ++ show pieces
  putStrLn $ "Partial: " ++ show partial
  
  -- Current backward strategy (from code)
  putStrLn "\nCurrent backward strategy (broken):"
  let edgePiece = last (init pieces)  -- last . init
      edgeLine = BS.append edgePiece partial  -- piece + partial
      middleLines = init (tail (init pieces))  -- init . tail . init
      newPartial = head pieces
  
  putStrLn $ "  Edge piece (last . init): " ++ show edgePiece
  putStrLn $ "  Edge line (piece + partial): " ++ show edgeLine
  putStrLn $ "  Middle (init . tail . init): " ++ show middleLines
  putStrLn $ "  New partial (head): " ++ show newPartial
  
  -- After prepend to empty list and adding partial
  let allLines = middleLines ++ [edgeLine]  -- RightPartial
      withPartial = partial : allLines  -- Add partial at front
  putStrLn $ "  All lines: " ++ show allLines
  putStrLn $ "  With partial: " ++ show withPartial
  putStrLn $ "  After reverse: " ++ show (reverse withPartial)
  putStrLn $ "Expected: [\"a\", \"\", \"b\"]"
testForwardExtraction = do
  putStrLn "\nFrom \"a\\n\\nb\\n\" (canonical)"
  let pieces = ["a", "", "b", ""]
      partial = ""
  
  putStrLn $ "Pieces: " ++ show pieces
  putStrLn $ "Partial: " ++ show partial
  
  -- Simulate forward strategy
  let edgePiece = head pieces  -- "a"
      edgeLine = BS.append partial edgePiece  -- "" + "a" = "a"
      middleLines = tail (init pieces)  -- tail ["a", "", "b"] = ["", "b"]
      allLines = edgeLine : middleLines  -- ["a", "", "b"]
      newPartial = last pieces  -- ""
  
  putStrLn $ "Edge piece (head): " ++ show edgePiece
  putStrLn $ "Edge line (partial + edge): " ++ show edgeLine
  putStrLn $ "Middle (tail . init): " ++ show middleLines
  putStrLn $ "All lines: " ++ show allLines
  putStrLn $ "New partial (last): " ++ show newPartial
  putStrLn $ "Expected: [\"a\", \"\", \"b\"]"
  putStrLn $ "Match? " ++ show (allLines == ["a", "", "b"])

-- Test individual strategy functions with various inputs
testForwardStrategyFunctions :: IO ()
testForwardStrategyFunctions = do
  putStrLn "\nTesting with pieces = [\"a\", \"\", \"b\", \"\"]"
  testStratFuncs ["a", "", "b", ""]
  
  putStrLn "\nTesting with pieces = [\"line1\", \"line2\", \"line3\", \"\"]"
  testStratFuncs ["line1", "line2", "line3", ""]
  
  putStrLn "\nTesting with pieces = [\"\", \"\", \"\", \"\"]  (only newlines)"
  testStratFuncs ["", "", "", ""]
  
  putStrLn "\nTesting with pieces = [\"single\", \"\"]"
  testStratFuncs ["single", ""]

testStratFuncs :: [BS.ByteString] -> IO ()
testStratFuncs pieces = do
  putStrLn $ "  Pieces: " ++ show pieces
  putStrLn $ "  head (edge): " ++ show (head pieces)
  putStrLn $ "  last (new partial): " ++ show (last pieces)
  putStrLn $ "  tail . init (middle): " ++ show (tail (init pieces))
  putStrLn $ "  init (drop last): " ++ show (init pieces)
  putStrLn $ "  tail (drop first): " ++ show (tail pieces)
