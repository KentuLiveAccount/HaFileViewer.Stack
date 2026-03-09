{-# LANGUAGE OverloadedStrings #-}

-- Deep dive: Check what getLinesFrom is returning for empty lines

import HaFileViewer.LineCache
import HaFileViewer.CUILogViewer.ViewState
import HaFileViewer.CUILogViewer.Operations as Ops
import HaFileViewer.BidirectionalScanner (Direction(..))
import qualified Data.Text as T

testFile :: FilePath
testFile = "test-onenote.log"

main :: IO ()
main = do
  putStrLn "=== Deep Dive: Empty Lines Analysis ==="
  putStrLn ""
  
  -- Open cache directly
  cache <- openLineCache testFile
  
  putStrLn "1. Total lines in file:"
  totalLines <- getTotalLines cache
  putStrLn $ "   " ++ show totalLines
  
  -- Get lines 25-35 directly
  putStrLn ""
  putStrLn "2. Getting lines 1-30 from start:"
  (linesFromStart, topPos, bottomPos) <- getLinesFromStart cache 30
  putStrLn $ "   Received " ++ show (length linesFromStart) ++ " lines"
  putStrLn ""
  putStrLn "   Content analysis:"
  mapM_ (\(i, (text, lineNum)) -> 
    let len = T.length text
        isEmpty = text == ""
        content = if isEmpty then "<EMPTY>" else T.take 50 text
        emptyMarker = if isEmpty then " ** EMPTY **" else ""
    in putStrLn $ "   " ++ show i ++ ". Line " ++ show lineNum ++ " len=" ++ show len ++ " " ++ T.unpack content ++ emptyMarker
    ) (zip [1..] linesFromStart)
  
  -- Now scroll step by step and check each new line
  putStrLn ""
  putStrLn "3. Scrolling down and checking each new line:"
  
  let scrollAndCheck vs count = do
        if count >= 10
          then return vs
          else do
            vs' <- Ops.scrollDown vs
            let (first', last', _) = getViewportInfo vs'
            let viewport = vsViewport vs'
            let (lineNum, text) = last viewport  -- The newly added line
            let isEmpty = text == ""
            let marker = if isEmpty then " *** EMPTY LINE RETURNED ***" else ""
            putStrLn $ "   Scroll " ++ show count ++ ": Got line " ++ show lineNum ++ " len=" ++ show (T.length text) ++ marker
            scrollAndCheck vs' (count + 1)
  
  vs0 <- Ops.initializeViewer testFile 25
  scrollAndCheck vs0 1
  
  putStrLn ""
  putStrLn "Done."
  closeLineCache cache

getViewportInfo :: ViewState -> (Integer, Integer, Int)
getViewportInfo vs =
  let viewport = vsViewport vs
      first = if null viewport then 0 else fst (head viewport)
      last' = if null viewport then 0 else fst (last viewport)
      count = length viewport
  in (first, last', count)
