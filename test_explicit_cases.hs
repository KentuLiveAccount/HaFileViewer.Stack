{-# LANGUAGE OverloadedStrings #-}
-- Test with explicit expected outputs for each case

import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.IO
import System.IO.Temp (withSystemTempFile)
import System.IO.MMap (mmapFileByteString)
import HaFileViewer.BidirectionalScanner
  ( scanLines
  , Direction(..)  -- Forward | Backward
  )

-- Helper to read from file using mmap
readFromFile :: FilePath -> Integer -> Integer -> IO BS.ByteString
readFromFile path offset size = do
  content <- mmapFileByteString path Nothing
  return $ BS.take (fromIntegral size) $ BS.drop (fromIntegral offset) content

-- Test case: file content -> direction -> offset -> count -> expected lines
data TestCase = TestCase
  { tcName :: String
  , tcContent :: String
  , tcDirection :: Direction
  , tcCount :: Int
  , tcExpected :: [String]
  }

testCases :: [TestCase]
testCases =
  -- Forward tests
  [ TestCase "F1: Basic forward from start"
      "a\nb\nc\n"
      Forward 3
      ["a", "b", "c"]
  
  , TestCase "F2: Forward middle lines"
      "a\nb\nc\nd\ne\n"
      Forward 2
      ["a", "b"]  -- from start
  
  , TestCase "F3: Forward with empty lines"
      "a\n\nb\n"
      Forward 3
      ["a", "", "b"]
  
  -- Backward tests
  , TestCase "B1: Basic backward from end"
      "a\nb\nc\n"
      Backward 2
      ["b", "c"]
  
  , TestCase "B2: Backward with empty lines"
      "a\n\nb\n"
      Backward 3
      ["a", "", "b"]
  
  , TestCase "B3: Backward simple case"
      "a\nb\nc\n"
      Backward 3
      ["a", "b", "c"]
  ]

runTestCase :: TestCase -> IO Bool
runTestCase tc = do
  withSystemTempFile "test_explicit.txt" $ \path h -> do
    hPutStr h (tcContent tc)
    hClose h
    
    -- Get file size and scan
    size <- fromIntegral . BS.length <$> BS.readFile path
    result <- scanLines (tcDirection tc) size (readFromFile path) (tcCount tc)
    let resultStrs = map T.unpack result
    let passed = resultStrs == tcExpected tc
    
    putStrLn $ "\n" ++ tcName tc
    putStrLn $ "  Input: " ++ show (tcContent tc)
    putStrLn $ "  Params: " ++ show (tcDirection tc) ++ " count=" ++ show (tcCount tc)
    putStrLn $ "  Expected: " ++ show (tcExpected tc)
    putStrLn $ "  Got:      " ++ show resultStrs
    putStrLn $ "  " ++ if passed then "PASS" else "FAIL"
    
    return passed

main :: IO ()
main = do
  results <- mapM runTestCase testCases
  let passed = length (filter id results)
  let total = length results
  putStrLn $ "\n" ++ replicate 40 '='
  putStrLn $ show passed ++ "/" ++ show total ++ " tests passed"
  if passed == total
    then putStrLn "All tests passed!"
    else putStrLn $ show (total - passed) ++ " tests failed"

