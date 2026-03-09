{-# LANGUAGE OverloadedStrings #-}

-- Test case for bug: Scroll down to line 100, then scroll back up stops before line 1
-- Expected: Should return to line 1
-- Actual: Stops at some line > 1

import HaFileViewer.LineCache
import HaFileViewer.CUILogViewer.ViewState
import HaFileViewer.CUILogViewer.Operations as Ops
import Control.Monad (foldM)
import System.IO (IOMode(..), withFile)
import qualified Data.Text as T

-- Test file path
testFile :: FilePath
testFile = "test-onenote.log"

-- Initialize viewer with the test file
initViewer :: IO ViewState
initViewer = Ops.initializeViewer testFile 25

-- Get viewport info helper
getViewportInfo :: ViewState -> (Integer, Integer, Int)
getViewportInfo vs =
  let viewport = vsViewport vs
      first = if null viewport then 0 else fst (head viewport)
      last' = if null viewport then 0 else fst (last viewport)
      count = length viewport
  in (first, last', count)

-- Debug helper: Print detailed viewport content
printViewportDebug :: String -> ViewState -> IO ()
printViewportDebug label vs = do
  putStrLn ""
  putStrLn $ "=== DEBUG: " ++ label ++ " ==="
  
  let viewport = vsViewport vs
      cursor = vsCursor vs
  
  -- Print cursor positions
  putStrLn "Cursor State:"
  putStrLn $ "  topPosition: (internal)"
  putStrLn $ "  bottomPosition: (internal)"
  putStrLn $ "  firstLine: " ++ show (cursorFirstLine cursor)
  putStrLn $ "  lastLine: " ++ show (cursorLastLine cursor)
  putStrLn $ "  origin: " ++ show (cursorOrigin cursor)
  
  -- Print viewport lines
  putStrLn "\nViewport Content:"
  putStrLn $ "Total lines in viewport: " ++ show (length viewport)
  
  let emptyCount = length (filter (\(_, txt) -> txt == "") viewport)
  putStrLn $ "Empty lines: " ++ show emptyCount
  
  putStrLn "\nLine Details:"
  mapM_ (\(lineNum, text) -> do
    let len = T.length text
        first50 = T.take 50 text
        isEmpty = text == ""
        emptyMarker = if isEmpty then " <EMPTY>" else ""
        lineStr = show lineNum
        lenStr = show len
    putStrLn $ "  Line " ++ lineStr ++ ": len=" ++ lenStr ++ " " ++ T.unpack first50 ++ emptyMarker
    ) viewport
  putStrLn ""

-- Test: Scroll down to line 100, then scroll back up
testScrollDownAndBackUp :: IO ()
testScrollDownAndBackUp = do
  putStrLn "=== Bug Reproduction Test ==="
  putStrLn "File: test-onenote.log"
  putStrLn ""
  
  vs0 <- initViewer
  let (first0, last0, _) = getViewportInfo vs0
  putStrLn $ "Initial viewport: " ++ show first0 ++ " to " ++ show last0
  
  -- Scroll down until line 100 is visible
  putStrLn "\nScrolling down until line 100 is visible..."
  let scrollDownUntil100 vs = do
        let (_, lastLine, _) = getViewportInfo vs
        if lastLine >= 100
          then return vs
          else do
            vs' <- Ops.scrollDown vs
            let (first', last', _) = getViewportInfo vs'
            putStrLn $ "  After scroll: lines " ++ show first' ++ " to " ++ show last'
            -- Check for empty lines being displayed
            let viewport' = vsViewport vs'
            let hasEmpty = any (\(_, txt) -> txt == "") viewport'
            if hasEmpty
              then putStrLn "  >>> FOUND EMPTY LINES IN VIEWPORT <<<"
              else return ()
            -- Add debug dump when we reach line 30
            if last' == 30
              then printViewportDebug "VIEWPORT AT LINE 30"  vs'
              else return ()
            scrollDownUntil100 vs'
  
  vs100 <- scrollDownUntil100 vs0
  let (first100, last100, _) = getViewportInfo vs100
  putStrLn $ "\nReached line 100. Viewport: " ++ show first100 ++ " to " ++ show last100
  
  -- Now scroll back up until we can't scroll anymore
  putStrLn "\nScrolling back up until we stop..."
  let scrollUpUntilStop vs prevFirst count = do
        vs' <- Ops.scrollUp vs
        let (first', last', _) = getViewportInfo vs'
        if first' == prevFirst
          then do
            putStrLn $ "  Stopped at line " ++ show first'
            return (vs', count)
          else do
            putStrLn $ "  After scroll up: lines " ++ show first' ++ " to " ++ show last'
            -- Check for empty lines
            let viewport' = vsViewport vs'
            let hasEmpty = any (\(_, txt) -> txt == "") viewport'
            if hasEmpty
              then putStrLn "  >>> FOUND EMPTY LINES IN VIEWPORT <<<"
              else return ()
            scrollUpUntilStop vs' first' (count + 1)
  
  (vsFinal, scrollCount) <- scrollUpUntilStop vs100 first100 0
  let (firstFinal, lastFinal, _) = getViewportInfo vsFinal
  
  putStrLn $ "\n=== Results ==="
  putStrLn $ "Scrolled up " ++ show scrollCount ++ " times"
  putStrLn $ "Final viewport: " ++ show firstFinal ++ " to " ++ show lastFinal
  
  -- Check the bug
  if firstFinal == 1
    then putStrLn "✓ PASS: Correctly returned to line 1"
    else do
      putStrLn $ "✗ FAIL: Stopped at line " ++ show firstFinal ++ " (expected line 1)"
      putStrLn $ "  Bug: Scroll up stopped " ++ show (firstFinal - 1) ++ " lines before reaching line 1"
  
  -- Also check viewport content for empty lines
  let viewport = vsViewport vsFinal
  let emptyLines = filter (\(ln, txt) -> txt == "") viewport
  if null emptyLines
    then putStrLn "✓ No empty lines in final viewport"
    else do
      putStrLn $ "✗ Found " ++ show (length emptyLines) ++ " empty lines in viewport:"
      mapM_ (\(ln, _) -> putStrLn $ "    Line " ++ show ln) emptyLines
  
  closeLineCache (vsCache vsFinal)

main :: IO ()
main = testScrollDownAndBackUp
