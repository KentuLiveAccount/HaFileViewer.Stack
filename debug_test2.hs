-- Quick debug script to see what Test #2 produces
import qualified Data.Text as T
import HaFileViewer.LineCache
import HaFileViewer.CUILogViewer.ViewState
import HaFileViewer.BidirectionalScanner (Direction(..))
import System.IO (writeFile)
import System.Directory (removeFile)

testFile :: FilePath
testFile = "debug_test2.txt"

createTestFile :: IO ()
createTestFile = writeFile testFile $ unlines [show i | i <- [1..100]]

simulateScrollDown :: ViewState -> IO ViewState
simulateScrollDown vs = do
  let cache = vsCache vs
      cursor = vsCursor vs
      viewport = vsViewport vs
  
  if null viewport
    then return vs
    else do
      (moreLines, newPosition) <- getLinesFrom cache (cursorPosition cursor) Forward 1
      
      if null moreLines
        then return vs
        else do
          let (text, lineNum) = head moreLines
              newLine = (lineNum, text)
              newViewport = shiftViewportDown viewport newLine (vsViewportSize vs)
              newCursor = cursor { cursorPosition = newPosition }
          
          return vs { vsViewport = newViewport, vsCursor = newCursor }

main :: IO ()
main = do
  createTestFile
  
  -- Initialize
  cache <- openLineCache testFile
  (initialLines, initialPosition) <- getLinesFromStart cache 25
  let swappedLines = [(lineNum, text) | (text, lineNum) <- initialLines]
      cursor = ViewCursor 
        { cursorPosition = initialPosition
        , cursorOrigin = lpOrigin initialPosition
        }
      vs0 = ViewState
        { vsCache = cache
        , vsCursor = cursor
        , vsViewport = swappedLines
        , vsViewportSize = 25
        , vsFilePath = testFile
        }
  
  -- Print initial state
  putStrLn "=== Initial State ==="
  putStrLn $ "First line: " ++ show (fst $ head $ vsViewport vs0)
  putStrLn $ "Last line: " ++ show (fst $ last $ vsViewport vs0)
  putStrLn $ "lpLineNum: " ++ show (lpLineNum (cursorPosition cursor))
  putStrLn ""
  
  -- Scroll down once
  vs1 <- simulateScrollDown vs0
  putStrLn "=== After Scroll Down Once ==="
  putStrLn $ "First line: " ++ show (fst $ head $ vsViewport vs1)
  putStrLn $ "Last line: " ++ show (fst $ last $ vsViewport vs1)
  putStrLn $ "lpLineNum: " ++ show (lpLineNum (cursorPosition $ vsCursor vs1))
  putStrLn $ "Expected first: 2, last: 26"
  putStrLn ""
  
  -- Show first 5 and last 5 lines
  putStrLn "First 5 lines:"
  mapM_ (\(num, _) -> putStrLn $ "  " ++ show num) $ take 5 $ vsViewport vs1
  putStrLn "Last 5 lines:"
  mapM_ (\(num, _) -> putStrLn $ "  " ++ show num) $ drop 20 $ vsViewport vs1
  
  closeLineCache cache
  removeFile testFile
