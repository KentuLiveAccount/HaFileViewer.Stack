{-# LANGUAGE OverloadedStrings #-}

-- Systematic UI testing for CUILogViewer
-- Tests all basic operations and verifies invariants
-- NOW USES ACTUAL OPERATIONS FROM Operations.hs (not simulated!)

module Main where

import qualified Data.Text as T
import HaFileViewer.Backend.LineCache
import HaFileViewer.CUILogViewer.ViewState
import HaFileViewer.Backend.BidirectionalScanner (Direction(..))
import System.IO (writeFile, openTempFile, hPutStr, hClose)
import System.Directory (removeFile, getCurrentDirectory, setCurrentDirectory)
import Control.Exception (bracket)
import Control.Monad (when, foldM)

-- Import actual operations - these will be the REAL functions from Operations module
import qualified HaFileViewer.CUILogViewer.Operations as Ops

-- Test file setup
testFile :: FilePath
testFile = "test_ui_systematic.txt"

shortFile :: FilePath
shortFile = "test_ui_short.txt"

-- Create test file with 100 numbered lines
createTestFile :: IO ()
createTestFile = do
  writeFile testFile $ unlines [show i | i <- [1..100]]
  writeFile shortFile $ unlines [show i | i <- [1..10]]  -- shorter than viewport (25)

cleanupTestFile :: IO ()
cleanupTestFile = do
  removeFile testFile
  removeFile shortFile

-- Helper to create initial state using Operations module
initializeViewer :: IO ViewState
initializeViewer = Ops.initializeViewer testFile 25

-- Now using real Operations module - no more simulated functions!
-- Direct aliases to Operations functions
simulateScrollDown :: ViewState -> IO ViewState
simulateScrollDown = Ops.scrollDown

simulateScrollUp :: ViewState -> IO ViewState  
simulateScrollUp = Ops.scrollUp

simulateJumpToEnd :: ViewState -> IO ViewState
simulateJumpToEnd = Ops.jumpToEnd

simulateJumpToStart :: ViewState -> IO ViewState
simulateJumpToStart = Ops.jumpToStart

-- Helper to extract viewport info
getViewportInfo :: ViewState -> (Integer, Integer, Int)
getViewportInfo vs = 
  let viewport = vsViewport vs
      firstLine = if null viewport then 0 else fst (head viewport)
      lastLine = if null viewport then 0 else fst (last viewport)
      count = length viewport
  in (firstLine, lastLine, count)

-- Helper to check if line numbers are consecutive
areConsecutive :: [Integer] -> Bool
areConsecutive [] = True
areConsecutive [_] = True
areConsecutive (a:b:rest) = abs (b - a) == 1 && areConsecutive (b:rest)

-- Test runner
runTest :: String -> IO Bool -> IO ()
runTest name test = do
  putStr $ name ++ "... "
  result <- test
  putStrLn $ if result then "[PASS]" else "[FAIL]"

-- ============================================================================
-- TEST SUITE
-- ============================================================================

-- Test 1: Initial state shows lines 1-25
testInitialState :: IO Bool
testInitialState = do
  vs <- initializeViewer
  let (first, last, count) = getViewportInfo vs
      lineNums = map fst (vsViewport vs)
  closeLineCache (vsCache vs)
  return $ first == 1 && last == 25 && count == 25 && areConsecutive lineNums

-- Test 2: Single scroll down shows lines 2-26
testSingleScrollDown :: IO Bool
testSingleScrollDown = do
  vs <- initializeViewer
  let initialFirst = cursorFirstLine $ vsCursor vs
      initialLast = cursorLastLine $ vsCursor vs
  putStrLn $ "  Initial viewport: " ++ show initialFirst ++ "-" ++ show initialLast
  vs' <- simulateScrollDown vs
  let (first, last, count) = getViewportInfo vs'
      lineNums = map fst (vsViewport vs')
      newFirst = cursorFirstLine $ vsCursor vs'
      newLast = cursorLastLine $ vsCursor vs'
  -- Debug output
  putStrLn $ "  DEBUG: first=" ++ show first ++ " last=" ++ show last ++ " count=" ++ show count
  putStrLn $ "  New viewport bounds: " ++ show newFirst ++ "-" ++ show newLast
  putStrLn $ "  Expected: first=2 last=26 count=25"
  closeLineCache (vsCache vs')
  return $ first == 2 && last == 26 && count == 25 && areConsecutive lineNums

-- Test 3: Down then Up returns to original state (Bug #6)
testDownThenUp :: IO Bool
testDownThenUp = do
  vs0 <- initializeViewer
  let original = getViewportInfo vs0
  putStrLn $ "  Initial: " ++ show original
  vs1 <- simulateScrollDown vs0
  let after_down = getViewportInfo vs1
  putStrLn $ "  After down: " ++ show after_down
  vs2 <- simulateScrollUp vs1
  let final = getViewportInfo vs2
      lineNums = map fst (vsViewport vs2)
  putStrLn $ "  After up: " ++ show final
  putStrLn $ "  Expected to match original: " ++ show original
  closeLineCache (vsCache vs2)
  return $ original == final && areConsecutive lineNums

-- Test 4: Up then Down returns to original state
testUpThenDown :: IO Bool
testUpThenDown = do
  vs0 <- initializeViewer
  -- Scroll down a few times first to be in the middle
  vs1 <- simulateScrollDown vs0
  vs2 <- simulateScrollDown vs1
  vs3 <- simulateScrollDown vs2
  let middle = getViewportInfo vs3
  -- Now up then down
  vs4 <- simulateScrollUp vs3
  vs5 <- simulateScrollDown vs4
  let final = getViewportInfo vs5
      lineNums = map fst (vsViewport vs5)
  closeLineCache (vsCache vs5)
  return $ middle == final && areConsecutive lineNums

-- Test 5: Jump to end shows correct negative lines
testJumpToEnd :: IO Bool
testJumpToEnd = do
  vs <- initializeViewer
  vs' <- simulateJumpToEnd vs
  let (first, last, count) = getViewportInfo vs'
      lineNums = map fst (vsViewport vs')
      origin = cursorOrigin (vsCursor vs')
  closeLineCache (vsCache vs')
  -- Should show lines -25 to -1
  return $ first == (-25) && last == (-1) && count == 25 
        && origin == FromEnd && areConsecutive lineNums

-- Test 6: Scroll up from end (Bug #5)
testScrollUpFromEnd :: IO Bool
testScrollUpFromEnd = do
  vs <- initializeViewer
  vs' <- simulateJumpToEnd vs
  vs'' <- simulateScrollUp vs'
  let (first, last, count) = getViewportInfo vs''
      lineNums = map fst (vsViewport vs'')
  closeLineCache (vsCache vs'')
  -- Should show lines -26 to -2
  return $ first == (-26) && last == (-2) && count == 25 && areConsecutive lineNums

-- Test 7: Scroll down from end (Bug #5)
testScrollDownFromEnd :: IO Bool
testScrollDownFromEnd = do
  vs <- initializeViewer
  vs' <- simulateJumpToEnd vs
  -- Try to scroll down (should stay at end or do nothing)
  vs'' <- simulateScrollDown vs'
  let (first, last, count) = getViewportInfo vs''
      lineNums = map fst (vsViewport vs'')
  closeLineCache (vsCache vs'')
  -- Should still show lines -25 to -1 (no change)
  return $ first == (-25) && last == (-1) && count == 25 && areConsecutive lineNums

-- Test 8: Jump to start from end
testJumpToStartFromEnd :: IO Bool
testJumpToStartFromEnd = do
  vs <- initializeViewer
  vs' <- simulateJumpToEnd vs
  vs'' <- simulateJumpToStart vs'
  let (first, last, count) = getViewportInfo vs''
      lineNums = map fst (vsViewport vs'')
      origin = cursorOrigin (vsCursor vs'')
  closeLineCache (vsCache vs'')
  return $ first == 1 && last == 25 && count == 25 
        && origin == FromStart && areConsecutive lineNums

-- Test 9: Multiple scrolls down
testMultipleScrollsDown :: IO Bool
testMultipleScrollsDown = do
  vs0 <- initializeViewer
  vs1 <- simulateScrollDown vs0
  vs2 <- simulateScrollDown vs1
  vs3 <- simulateScrollDown vs2
  vs4 <- simulateScrollDown vs3
  vs5 <- simulateScrollDown vs4  -- 5 scrolls down
  let (first, last, count) = getViewportInfo vs5
      lineNums = map fst (vsViewport vs5)
  closeLineCache (vsCache vs5)
  -- Should show lines 6-30
  return $ first == 6 && last == 30 && count == 25 && areConsecutive lineNums

-- Test 10: Multiple scrolls down then same number up
testMultipleScrollsReverse :: IO Bool
testMultipleScrollsReverse = do
  vs0 <- initializeViewer
  let original = getViewportInfo vs0
  -- Go down 5 times
  vs1 <- simulateScrollDown vs0
  vs2 <- simulateScrollDown vs1
  vs3 <- simulateScrollDown vs2
  vs4 <- simulateScrollDown vs3
  vs5 <- simulateScrollDown vs4
  -- Go up 5 times
  vs6 <- simulateScrollUp vs5
  vs7 <- simulateScrollUp vs6
  vs8 <- simulateScrollUp vs7
  vs9 <- simulateScrollUp vs8
  vs10 <- simulateScrollUp vs9
  let final = getViewportInfo vs10
      lineNums = map fst (vsViewport vs10)
  closeLineCache (vsCache vs10)
  return $ original == final && areConsecutive lineNums

-- Test 11: No duplicate lines in viewport
testNoDuplicates :: IO Bool
testNoDuplicates = do
  vs0 <- initializeViewer
  vs1 <- simulateScrollDown vs0
  vs2 <- simulateScrollDown vs1
  let lineNums = map fst (vsViewport vs2)
      unique = length lineNums == length (nub' lineNums)
  closeLineCache (vsCache vs2)
  return unique
  where
    nub' [] = []
    nub' (x:xs) = x : nub' (filter (/= x) xs)

-- Test 12: Viewport bounds match actual viewport
testLineNumConsistency :: IO Bool
testLineNumConsistency = do
  vs <- initializeViewer
  let viewport = vsViewport vs
      cursor = vsCursor vs
      firstInViewport = if null viewport then 0 else fst (head viewport)
      lastInViewport = if null viewport then 0 else fst (last viewport)
      cursorFirst = cursorFirstLine cursor
      cursorLast = cursorLastLine cursor
  closeLineCache (vsCache vs)
  -- Viewport bounds should match cursor's tracked line numbers
  return $ firstInViewport == cursorFirst && lastInViewport == cursorLast

-- Test 13: Origin consistency after scrolling
testOriginConsistency :: IO Bool
testOriginConsistency = do
  vs0 <- initializeViewer
  let origin0 = cursorOrigin (vsCursor vs0)
  vs1 <- simulateScrollDown vs0
  vs2 <- simulateScrollDown vs1
  vs3 <- simulateScrollUp vs2
  let origin3 = cursorOrigin (vsCursor vs3)
  closeLineCache (vsCache vs3)
  -- Origin should never change during scrolling
  return $ origin0 == origin3 && origin0 == FromStart

-- Test 14: Origin changes only on jump
testOriginChangeOnJump :: IO Bool
testOriginChangeOnJump = do
  vs0 <- initializeViewer
  let origin0 = cursorOrigin (vsCursor vs0)
  vs1 <- simulateJumpToEnd vs0
  let origin1 = cursorOrigin (vsCursor vs1)
  vs2 <- simulateJumpToStart vs1
  let origin2 = cursorOrigin (vsCursor vs2)
  closeLineCache (vsCache vs2)
  return $ origin0 == FromStart && origin1 == FromEnd && origin2 == FromStart

-- Test 15: Reversibility - N downs then N ups (from middle)
testReversibilityDownUp :: IO Bool
testReversibilityDownUp = do
  vs0 <- initializeViewer
  -- Move to middle first (scroll down 10 times)
  vs1 <- foldM (\s _ -> simulateScrollDown s) vs0 [1..10]
  let middle = getViewportInfo vs1
  -- Now do 5 downs then 5 ups
  vs2 <- foldM (\s _ -> simulateScrollDown s) vs1 [1..5]
  vs3 <- foldM (\s _ -> simulateScrollUp s) vs2 [1..5]
  let final = getViewportInfo vs3
      lineNums = map fst (vsViewport vs3)
  closeLineCache (vsCache vs3)
  return $ middle == final && areConsecutive lineNums

-- Test 16: Reversibility - N ups then N downs (from middle)
testReversibilityUpDown :: IO Bool
testReversibilityUpDown = do
  vs0 <- initializeViewer
  -- Move to middle first (scroll down 20 times)
  vs1 <- foldM (\s _ -> simulateScrollDown s) vs0 [1..20]
  let middle = getViewportInfo vs1
  -- Now do 5 ups then 5 downs
  vs2 <- foldM (\s _ -> simulateScrollUp s) vs1 [1..5]
  vs3 <- foldM (\s _ -> simulateScrollDown s) vs2 [1..5]
  let final = getViewportInfo vs3
      lineNums = map fst (vsViewport vs3)
  closeLineCache (vsCache vs3)
  return $ middle == final && areConsecutive lineNums

-- Test 17: Reversibility from end - N ups then N downs
testReversibilityFromEnd :: IO Bool
testReversibilityFromEnd = do
  vs0 <- initializeViewer
  vs1 <- simulateJumpToEnd vs0
  let endState = getViewportInfo vs1
  -- Do 5 ups then 5 downs
  vs2 <- foldM (\s _ -> simulateScrollUp s) vs1 [1..5]
  vs3 <- foldM (\s _ -> simulateScrollDown s) vs2 [1..5]
  let final = getViewportInfo vs3
      lineNums = map fst (vsViewport vs3)
  closeLineCache (vsCache vs3)
  return $ endState == final && areConsecutive lineNums

-- Test 18: Boundary - Up at start does nothing
testUpAtStartDoesNothing :: IO Bool
testUpAtStartDoesNothing = do
  vs0 <- initializeViewer
  let start = getViewportInfo vs0
  vs1 <- simulateScrollUp vs0
  let afterUp = getViewportInfo vs1
  closeLineCache (vsCache vs1)
  return $ start == afterUp

-- Test 19: Boundary - Down at end does nothing
testDownAtEndDoesNothing :: IO Bool
testDownAtEndDoesNothing = do
  vs0 <- initializeViewer
  vs1 <- simulateJumpToEnd vs0
  -- Scroll down past the end (should reach a point where it stops)
  vs2 <- foldM (\s _ -> simulateScrollDown s) vs1 [1..10]
  let end1 = getViewportInfo vs2
  vs3 <- simulateScrollDown vs2
  let end2 = getViewportInfo vs3
  closeLineCache (vsCache vs3)
  -- After reaching end, more downs should not change state
  return $ end1 == end2

-- Test 20: Arrow keys work after jump to end (Bug discovered in manual testing)
testArrowKeysAfterJumpToEnd :: IO Bool
testArrowKeysAfterJumpToEnd = do
  vs0 <- initializeViewer
  -- Jump to end with G
  vs1 <- simulateJumpToEnd vs0
  let (endFirst, endLast, endCount) = getViewportInfo vs1
  
  -- Try scrolling up - should work
  vs2 <- simulateScrollUp vs1
  let (upFirst, upLast, upCount) = getViewportInfo vs2
  
  -- Should have moved up (first line more negative)
  let upWorked = upFirst < endFirst && upCount == 25
  
  -- Try scrolling down from end - should do nothing (at EOF)
  vs3 <- simulateScrollDown vs1
  let (downFirst, downLast, downCount) = getViewportInfo vs3
  
  -- Should stay at same position (at EOF)
  let downStaysAtEnd = downFirst == endFirst && downLast == endLast
  
  closeLineCache (vsCache vs3)
  return $ upWorked && downStaysAtEnd

-- ============================================================================
-- BOUNDARY TESTS: Short file (10 lines) with viewport size 25
-- ============================================================================

initializeShortViewer :: IO ViewState
initializeShortViewer = Ops.initializeViewer shortFile 25

-- Test 22: Short file loads fewer lines than viewport size
testShortFileLoadsPartial :: IO Bool
testShortFileLoadsPartial = do
  vs <- initializeShortViewer
  let (first, last, count) = getViewportInfo vs
  closeLineCache (vsCache vs)
  return $ first == 1 && last == 10 && count == 10  -- 10 lines, not 25

-- Test 23: pageDown on short file does nothing (already at EOF)
testPageDownShortFileDoesNothing :: IO Bool
testPageDownShortFileDoesNothing = do
  vs0 <- initializeShortViewer
  let before = getViewportInfo vs0
  vs1 <- Ops.pageDown vs0
  let after = getViewportInfo vs1
  closeLineCache (vsCache vs1)
  return $ before == after

-- Test 24: pageUp on short file does nothing (already at BOF)
testPageUpShortFileDoesNothing :: IO Bool
testPageUpShortFileDoesNothing = do
  vs0 <- initializeShortViewer
  let before = getViewportInfo vs0
  vs1 <- Ops.pageUp vs0
  let after = getViewportInfo vs1
  closeLineCache (vsCache vs1)
  return $ before == after

-- Test 25: pageDown near EOF returns partial page
testPageDownPartialPage :: IO Bool
testPageDownPartialPage = do
  -- 100-line file, viewport 25. Page down 3x gets to lines 76-100.
  -- Page down again: 0 lines remain, should stay put.
  vs0 <- initializeViewer
  vs1 <- Ops.pageDown vs0  -- 26-50
  vs2 <- Ops.pageDown vs1  -- 51-75
  vs3 <- Ops.pageDown vs2  -- 76-100
  let (first3, _, _) = getViewportInfo vs3
  vs4 <- Ops.pageDown vs3  -- at EOF, should not move
  let (first4, last4, count4) = getViewportInfo vs4
  closeLineCache (vsCache vs4)
  return $ first3 == first4 && last4 == 100 && count4 == 25

-- Test 26: pageUp near BOF returns partial page
testPageUpPartialPage :: IO Bool
testPageUpPartialPage = do
  -- Page down once to lines 26-50, then page up: should go back to 1-25
  vs0 <- initializeViewer
  vs1 <- Ops.pageDown vs0   -- 26-50
  vs2 <- Ops.pageDown vs1   -- 51-75
  vs3 <- Ops.pageUp vs2     -- 26-50
  vs4 <- Ops.pageUp vs3     -- 1-25
  let (first, last, count) = getViewportInfo vs4
  closeLineCache (vsCache vs4)
  return $ first == 1 && last == 25 && count == 25

-- ============================================================================
-- IO ERROR PROPAGATION TESTS
-- ============================================================================

-- Test 27: applyLoad with LoadFailed preserves viewport and sets vsError
testApplyLoadWithLoadFailed :: IO Bool
testApplyLoadWithLoadFailed = do
  vs <- initializeViewer
  let result = applyLoad Nothing vs (LoadFailed "test error")
  closeLineCache (vsCache vs)
  return $ vsViewport result == vsViewport vs && vsError result == Just "test error"

-- Test 28: applyScrollDown with LoadFailed preserves viewport and sets vsError
testApplyScrollDownWithLoadFailed :: IO Bool
testApplyScrollDownWithLoadFailed = do
  vs <- initializeViewer
  let result = applyScrollDown vs (LoadFailed "disk error")
  closeLineCache (vsCache vs)
  return $ vsViewport result == vsViewport vs && vsError result == Just "disk error"

-- Test 29: applyScrollUp with LoadFailed preserves viewport and sets vsError
testApplyScrollUpWithLoadFailed :: IO Bool
testApplyScrollUpWithLoadFailed = do
  vs <- initializeViewer
  let result = applyScrollUp vs (LoadFailed "network error")
  closeLineCache (vsCache vs)
  return $ vsViewport result == vsViewport vs && vsError result == Just "network error"

-- Test 30: applyLoad with LinesLoaded clears vsError
testApplyLoadClearsError :: IO Bool
testApplyLoadClearsError = do
  vs <- initializeViewer
  let vsWithError = vs { vsError = Just "old error" }
  linesResult <- getLinesFromStart (vsCache vs) 5
  let result = applyLoad Nothing vsWithError linesResult
  closeLineCache (vsCache vs)
  return $ vsError result == Nothing

-- Test 31: scrollDown on deleted file sets vsError in returned ViewState
testScrollDownOnDeletedFile :: IO Bool
testScrollDownOnDeletedFile = do
  (path, h) <- openTempFile "." "test-ioerror.txt"
  hPutStr h $ unlines [show i | i <- [1..10 :: Int]]
  hClose h
  vs <- Ops.initializeViewer path 5
  closeLineCache (vsCache vs)
  removeFile path
  vs' <- Ops.scrollDown vs
  return $ vsError vs' /= Nothing

-- ============================================================================
-- MAIN
-- ============================================================================

main :: IO ()
main = bracket
  (createTestFile >> putStrLn "Test file created")
  (\_ -> cleanupTestFile >> putStrLn "\nTest file cleaned up")
  (\_ -> do
    putStrLn "\n=== Systematic UI Test Suite ==="
    putStrLn "================================\n"
    
    runTest "01. Initial state (lines 1-25)" testInitialState
    runTest "02. Single scroll down (lines 2-26)" testSingleScrollDown
    runTest "03. Down then Up returns to start (Bug #6)" testDownThenUp
    runTest "04. Up then Down returns to middle" testUpThenDown
    runTest "05. Jump to end shows -25 to -1" testJumpToEnd
    runTest "06. Scroll up from end shows -26 to -2 (Bug #5)" testScrollUpFromEnd
    runTest "07. Scroll down from end stays at -25 to -1" testScrollDownFromEnd
    runTest "08. Jump to start from end" testJumpToStartFromEnd
    runTest "09. Multiple scrolls down (5x)" testMultipleScrollsDown
    runTest "10. Multiple scrolls reversible (5 down + 5 up)" testMultipleScrollsReverse
    runTest "11. No duplicate lines in viewport" testNoDuplicates
    runTest "12. Viewport bounds match cursor tracking" testLineNumConsistency
    runTest "13. Origin stays constant during scroll" testOriginConsistency
    runTest "14. Origin changes on jump commands" testOriginChangeOnJump
    
    putStrLn "\n--- Reversibility Properties ---"
    runTest "15. Reversible: 5 down + 5 up (from middle)" testReversibilityDownUp
    runTest "16. Reversible: 5 up + 5 down (from middle)" testReversibilityUpDown
    runTest "17. Reversible: 5 up + 5 down (from end)" testReversibilityFromEnd
    
    putStrLn "\n--- Boundary Conditions ---"
    runTest "18. Up at start does nothing" testUpAtStartDoesNothing
    runTest "19. Down at end does nothing" testDownAtEndDoesNothing
    runTest "20. Arrow keys work after jump to end" testArrowKeysAfterJumpToEnd
    runTest "21. Page down 2x then page up 2x returns to start" testPageNavigation

    putStrLn "\n--- Short File (10 lines, viewport 25) ---"
    runTest "22. Short file loads partial viewport" testShortFileLoadsPartial
    runTest "23. pageDown on short file does nothing" testPageDownShortFileDoesNothing
    runTest "24. pageUp on short file does nothing" testPageUpShortFileDoesNothing
    runTest "25. pageDown at EOF does nothing" testPageDownPartialPage
    runTest "26. pageUp returns full page from near-BOF" testPageUpPartialPage
    
    putStrLn "\n--- IO Error Propagation ---"
    runTest "27. applyLoad with LoadFailed preserves viewport and sets vsError" testApplyLoadWithLoadFailed
    runTest "28. applyScrollDown with LoadFailed preserves viewport and sets vsError" testApplyScrollDownWithLoadFailed
    runTest "29. applyScrollUp with LoadFailed preserves viewport and sets vsError" testApplyScrollUpWithLoadFailed
    runTest "30. applyLoad with LinesLoaded clears vsError" testApplyLoadClearsError
    runTest "31. scrollDown on deleted file sets vsError" testScrollDownOnDeletedFile
    
    putStrLn "\n================================"
  )

-- Test page navigation reversibility (Bug found: page down 2x, page up 2x shows line 50)
testPageNavigation :: IO Bool
testPageNavigation = do
  vs0 <- initializeViewer
  let (init_first, init_last, _) = getViewportInfo vs0
  
  -- Page down twice
  vs1 <- Ops.pageDown vs0
  vs2 <- Ops.pageDown vs1
  let (after_down_first, after_down_last, _) = getViewportInfo vs2
  
  -- Page up twice - should return to lines 1-25
  vs3 <- Ops.pageUp vs2
  vs4 <- Ops.pageUp vs3
  let (final_first, final_last, _) = getViewportInfo vs4
  
  closeLineCache (vsCache vs4)
  
  -- Should be back at initial position
  return $ final_first == init_first && final_last == init_last
