# Bug Report: Scroll Regression with Large File

**Date:** 2026-03-09  
**File:** test-onenote.log (20MB, 71,356 lines)  
**Status:** REPRODUCED

---

## Symptoms

### 1. Empty Lines Appearing in Viewport (Scrolling Down)
- Start at line 1-25
- Scroll down repeatedly
- **BUG:** Empty lines appear in viewport starting around line 3-27
- Empty lines continue appearing on every subsequent scroll down
- File does NOT contain empty lines (verified with PowerShell Get-Content)

### 2. Scroll Up Stops Prematurely
- Scroll down to line 100 (76+ scrolls)
- Scroll back up repeatedly until scroll stops
- **BUG:** Stops at line 15 instead of line 1
- Expected: Should return to line 1
- Actual: Stops 14 lines short of the start

---

## Reproduction

### Test Case: test_bug_scroll_regression.hs

```bash
cd C:\GitHub\HaFileViewer.Stack
stack runhaskell test_bug_scroll_regression.hs
```

**Results:**
```
Initial viewport: 1 to 25
Scrolling down until line 100 is visible...
  After scroll: lines 2 to 26
  After scroll: lines 3 to 27
  >>> FOUND EMPTY LINES IN VIEWPORT <<<  # Bug starts here!
  ... (empty lines on every scroll) ...
  After scroll: lines 76 to 100

Reached line 100. Viewport: 76 to 100

Scrolling back up until we stop...
  ... (61 scroll ups) ...
  Stopped at line 15  # Bug! Should be line 1

=== Results ===
Scrolled up 61 times
Final viewport: 15 to 39
✗ FAIL: Stopped at line 15 (expected line 1)
  Bug: Scroll up stopped 14 lines before reaching line 1
```

---

## Analysis

### Empty Lines Issue
- Empty text entries ("") appear in vsViewport
- File itself has no empty lines (verified)
- Appears to be a display/caching bug
- Starts appearing after ~3 scrolls down

### Scroll Up Stops Early
- Should stop when `cursorFirstLine == 1`
- Actually stops at `cursorFirstLine == 15`
- Suggests boundary check or line counting bug
- Related to the empty lines issue?

---

## Hypothesis

**Possible causes:**
1. Line counting error in cache/scanner
2. Empty lines being inserted during scroll operations
3. Boundary detection failing to recognize file start
4. Cache corruption during bidirectional scrolling

**Next steps:**
1. Debug why empty lines appear in viewport
2. Check line number calculations in scrollDown
3. Verify scrollUp boundary condition logic
4. Inspect cache state after scrolls

---

## Test Files

- **Test script:** `test_bug_scroll_regression.hs`
- **Test data:** `test-onenote.log` (20MB OneNote diagnostic log)
- **Original:** `C:\Users\kentu\AppData\Local\Temp\Diagnostics\ONENOTE\Primary*.log`

---

## Impact

**Severity:** HIGH
- Breaks basic scrolling functionality
- Affects large files (20MB+, 70K+ lines)
- Makes viewer unusable for production logs
- Silently corrupts display (empty lines shown)
- Prevents returning to file start

**User Impact:**
- Cannot trust line numbers
- Cannot return to file beginning
- Phantom empty lines confuse log analysis
