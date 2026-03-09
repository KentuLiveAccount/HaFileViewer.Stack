# Systematic UI Test Results

## Summary
- **Total Tests**: 20
- **Passed**: 17
- **Failed**: 3
- **Pass Rate**: 85%

---

## Test Results Detail

### ✅ PASSING TESTS (17)

| # | Test Name | Result | Details |
|---|-----------|--------|---------|
| 01 | Initial state (lines 1-25) | PASS | Opens file showing lines 1-25 correctly |
| 02 | Single scroll down (lines 2-26) | PASS | Scrolling down increments viewport by 1 line |
| 03 | Down then Up returns to start (Bug #6) | PASS | Reversibility confirmed |
| 04 | Up then Down returns to middle | PASS | Reversibility confirmed |
| 05 | Jump to end shows -25 to -1 | PASS | Negative indexing from end works |
| 06 | Scroll up from end shows -26 to -2 (Bug #5) | PASS | Can scroll backwards from end |
| 08 | Jump to start from end | PASS | Jump back to start works |
| 09 | Multiple scrolls down (5x) | PASS | Shows lines 6-30 after 5 scrolls |
| 10 | Multiple scrolls reversible (5 down + 5 up) | PASS | Reversible scrolling works |
| 11 | No duplicate lines in viewport | PASS | All viewport lines are unique |
| 12 | Viewport bounds match cursor tracking | PASS | State tracking is consistent |
| 13 | Origin stays constant during scroll | PASS | Origin doesn't change with scroll operations |
| 14 | Origin changes on jump commands | PASS | Origin correctly changes with jump operations |
| 15 | Reversible: 5 down + 5 up (from middle) | PASS | Complex reversibility from middle position |
| 16 | Reversible: 5 up + 5 down (from middle) | PASS | Complex reversibility in reverse order |
| 17 | Reversible: 5 up + 5 down (from end) | PASS | Complex reversibility from end position |
| 18 | Up at start does nothing | PASS | Boundary condition: up at file start is safe |

---

## ❌ FAILING TESTS (3)

### Test 07: Scroll down from end stays at -25 to -1
**Expected Behavior**: When at the end of file (showing lines -25 to -1), scrolling down should stay at the same position since we're already at EOF.

**Current Behavior**: [FAIL] - Test is failing, indicating scrollDown from the end is not behaving as expected.

**Expected Values**:
- first = -25
- last = -1
- count = 25
- Viewport should remain unchanged

**Test Code** (lines 165-176):
```haskell
testScrollDownFromEnd :: IO Bool
testScrollDownFromEnd = do
  vs <- initializeViewer
  vs' <- simulateJumpToEnd vs
  vs'' <- simulateScrollDown vs'
  let (first, last, count) = getViewportInfo vs''
      lineNums = map fst (vsViewport vs'')
  closeLineCache (vsCache vs'')
  return $ first == (-25) && last == (-1) && count == 25 && areConsecutive lineNums
```

---

### Test 19: Down at end does nothing
**Expected Behavior**: After jumping to the end and scrolling down 10 times, the viewport should stabilize and not change further. Any additional scrollDown operations should have no effect.

**Current Behavior**: [FAIL] - The viewport is still changing after reaching the end.

**Test Code** (lines 335-347):
```haskell
testDownAtEndDoesNothing :: IO Bool
testDownAtEndDoesNothing = do
  vs0 <- initializeViewer
  vs1 <- simulateJumpToEnd vs0
  vs2 <- foldM (\s _ -> simulateScrollDown s) vs1 [1..10]  -- 10 scrolls down
  let end1 = getViewportInfo vs2
  vs3 <- simulateScrollDown vs2
  let end2 = getViewportInfo vs3
  closeLineCache (vsCache vs3)
  return $ end1 == end2  -- Should be unchanged
```

**Issue**: After multiple scrollDown operations from the end position, another scrollDown causes a change when it shouldn't.

---

### Test 20: Arrow keys work after jump to end
**Expected Behavior**: 
1. Jump to end (showing lines -25 to -1)
2. ScrollUp should move to -26 to -2 (more negative)
3. ScrollDown from end should stay at -25 to -1 (no change)

**Current Behavior**: [FAIL] - One or both of these conditions are failing.

**Test Code** (lines 349-372):
```haskell
testArrowKeysAfterJumpToEnd :: IO Bool
testArrowKeysAfterJumpToEnd = do
  vs0 <- initializeViewer
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
```

**Conditions Being Tested**:
- `upWorked`: upFirst < endFirst (scrollUp makes first line more negative) ✓ or ✗
- `downStaysAtEnd`: downFirst == endFirst && downLast == endLast (scrollDown stays at EOF) ✓ or ✗

---

## Debug Output Examples

### Test 02 Debug Output (PASS)
```
Initial viewport: 1-25
DEBUG: first=2 last=26 count=25
New viewport bounds: 2-26
Expected: first=2 last=26 count=25
```

### Test 03 Debug Output (PASS)
```
Initial: (1,25,25)
After down: (2,26,25)
After up: (1,25,25)
Expected to match original: (1,25,25)
```

---

## Root Cause Analysis

The failing tests all relate to **boundary behavior at EOF (End of File)**:

1. **Tests 07 & 19** - The `scrollDown` operation when already at the end should be a no-op, but it's currently modifying the viewport.

2. **Test 20** - The `scrollDown` from the end position is causing state changes when it should remain stable.

All three failures are manifestations of the same root issue: **scrollDown doesn't properly check if the viewer is at EOF and should not advance further.**

---

## Affected Operations
- `Operations.scrollDown` - Not properly handling EOF boundary condition

## Related Bug References
- Bug #5: Scroll operations from end (partially fixed)
- Bug #6: Down then Up reversibility (fixed)

---

## Next Steps
1. Fix the EOF boundary check in `Operations.scrollDown`
2. Ensure scrollDown returns unchanged state when already at EOF
3. Re-run test suite to confirm all 20 tests pass
