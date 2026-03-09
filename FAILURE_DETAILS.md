# Detailed Failure Analysis - UI Systematic Tests

## Overview
3 out of 20 tests failed. All failures are related to EOF (End of File) boundary handling in the `scrollDown` operation.

---

## FAILURE 1: Test 07 - "Scroll down from end stays at -25 to -1"

### Test Code
```haskell
testScrollDownFromEnd :: IO Bool
testScrollDownFromEnd = do
  vs <- initializeViewer                    -- Create viewer with 100 lines
  vs' <- simulateJumpToEnd vs               -- Jump to end
  vs'' <- simulateScrollDown vs'            -- Try to scroll down from end
  let (first, last, count) = getViewportInfo vs''
      lineNums = map fst (vsViewport vs'')
  closeLineCache (vsCache vs'')
  return $ first == (-25) && last == (-1) && count == 25 && areConsecutive lineNums
```

### What Should Happen
1. Initialize viewer with lines 1-100
2. Jump to end → viewport shows lines **-25 to -1** (last 25 lines)
3. Try scrollDown from end
4. Expected: No change → still shows lines **-25 to -1**

### What Is Happening
- **Test Result**: [FAIL]
- **Symptom**: The viewport is changing when `scrollDown` is called from the end position
- **Expected State**: first=-25, last=-1, count=25
- **Actual State**: Unknown (but not matching expected)

### Root Cause
The `scrollDown` function does not check if the viewer is already at the end of file before attempting to advance the viewport.

---

## FAILURE 2: Test 19 - "Down at end does nothing"

### Test Code
```haskell
testDownAtEndDoesNothing :: IO Bool
testDownAtEndDoesNothing = do
  vs0 <- initializeViewer
  vs1 <- simulateJumpToEnd vs0
  
  -- Perform 10 consecutive scrollDown operations from the end
  vs2 <- foldM (\s _ -> simulateScrollDown s) vs1 [1..10]
  let end1 = getViewportInfo vs2
  
  -- One more scrollDown
  vs3 <- simulateScrollDown vs2
  let end2 = getViewportInfo vs3
  
  closeLineCache (vsCache vs3)
  -- After reaching stable end state, further scrollDowns should not change anything
  return $ end1 == end2  -- FAILS HERE
```

### What Should Happen
1. Initialize viewer with 100 lines
2. Jump to end
3. Perform 10 consecutive scrollDown operations
4. Perform 1 more scrollDown
5. Expected: State from operation 10 == State from operation 11

### What Is Happening
- **Test Result**: [FAIL]
- **Symptom**: After 10 scrollDown operations from EOF, one more scrollDown causes a state change
- **Expected**: State remains unchanged
- **Actual**: State changes

---

## FAILURE 3: Test 20 - "Arrow keys work after jump to end"

### Test Code
```haskell
testArrowKeysAfterJumpToEnd :: IO Bool
testArrowKeysAfterJumpToEnd = do
  vs0 <- initializeViewer
  vs1 <- simulateJumpToEnd vs0
  let (endFirst, endLast, endCount) = getViewportInfo vs1
  
  -- Test scrollUp - should work
  vs2 <- simulateScrollUp vs1
  let (upFirst, upLast, upCount) = getViewportInfo vs2
  let upWorked = upFirst < endFirst && upCount == 25
  
  -- Test scrollDown - should do nothing at EOF
  vs3 <- simulateScrollDown vs1
  let (downFirst, downLast, downCount) = getViewportInfo vs3
  let downStaysAtEnd = downFirst == endFirst && downLast == endLast
  
  closeLineCache (vsCache vs3)
  return $ upWorked && downStaysAtEnd  -- FAILS HERE
```

### What Should Happen
1. Jump to end → viewport [-25 to -1]
2. ScrollUp → viewport [-26 to -2] (upFirst < endFirst)
3. ScrollDown from end → viewport stays [-25 to -1] (downStaysAtEnd)

### What Is Happening
- **Test Result**: [FAIL]
- **Symptom**: scrollDown from end is not keeping the viewport at the same position
- **Failing Condition**: downStaysAtEnd = (downFirst == -25 && downLast == -1)
- **Actual**: downFirst and/or downLast are changing

---

## Common Root Cause: EOF Boundary Not Checked in scrollDown

All three failures indicate the same issue:

**The `scrollDown` operation does not properly check if the viewer is already at the end of the file.**

### What Needs to Happen
```
When scrollDown is called and the viewport already shows 
the last N lines of the file:
  → Return the ViewState unchanged
  → Do NOT advance the viewport further
  
When scrollDown is called from a position that can scroll:
  → Advance the viewport normally
```

### Where to Fix
**File**: `app/CUILogViewer/Operations.hs`
**Function**: `scrollDown`

The function needs to add a check: "Are we already at EOF?"

### Expected Fix Pattern
```haskell
scrollDown :: ViewState -> IO ViewState
scrollDown vs = do
  -- [Step 1] Check if we're already at end of file
  -- If yes: return vs (no-op)
  -- [Step 2] If no: proceed with normal advance logic
```

---

## Quick Reference: What Each Test Is Checking

| Test | Checks | Fails Because |
|------|--------|---------------|
| 07 | Single scrollDown from end | scrollDown changes viewport instead of no-op |
| 19 | Multiple scrollDowns from end are all no-ops | scrollDown keeps changing state |
| 20 | ScrollUp works, ScrollDown is no-op at end | scrollDown not staying at EOF |

---

## Expected Behavior After Fix

```
File with 100 lines, viewport size 25

[BEFORE FIX]
Jump to end:        [-25 to -1] ✓
ScrollDown:         [CHANGES!] ✗
ScrollDown again:   [CHANGES MORE!] ✗

[AFTER FIX]
Jump to end:        [-25 to -1] ✓
ScrollDown:         [-25 to -1] ✓ (no-op)
ScrollDown again:   [-25 to -1] ✓ (no-op)
ScrollUp:           [-26 to -2] ✓ (works)
```

---

## Verification Steps

1. Run test 07 - Should PASS
2. Run test 19 - Should PASS
3. Run test 20 - Should PASS
4. Run full test suite - All 20 should PASS
5. Manual test: Scroll to end of large file, confirm arrow keys work
