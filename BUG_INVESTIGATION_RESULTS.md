# Bug Investigation Results: Scroll Regression

**Date:** 2026-03-09  
**Investigation Method:** Agent pair analysis + Debug scripting  
**Status:** Root cause identified

---

## Investigation Summary

### Agent 1 (Explore): Code Analysis & Hypotheses

**4 Hypotheses Generated:**

#### HYPOTHESIS 1: Offset/Line Number Mismatch in Bidirectional Scanning ⚠️ HIGH CONFIDENCE

**Evidence:**
- `BidirectionalScanner.lhs:406-408` calculates backward line numbers incorrectly:
  ```haskell
  Backward -> reverse [startLineNum - fromIntegral count + 1 .. startLineNum]
  ```
- Returns line numbers in reverse order, but scanner returns lines in file order
- Mismatch causes wrong line number → offset pairing in `LineCache.lhs:347`

**Impact:**
- Wrong line numbers cause cache lookups to fail → empty lines
- Corrupted numbering causes boundary check failure → stops at line 15

#### HYPOTHESIS 2: Line Number Calculation Accumulation Error ⚠️ HIGH CONFIDENCE

**Evidence:**
- Each scroll operation calculates line numbers independently
- Errors accumulate over multiple scrolls (76 down, 61 up)
- After ~14 scrolls, accumulation reaches 14-line drift
- Cursor's `cursorFirstLine`/`cursorLastLine` become unreliable

**Pattern:**
- Scroll down 76 times: Each miscalculation adds small error
- Scroll up 61 times: Errors compound
- Final position: 14 lines off from expected

#### HYPOTHESIS 3: Empty Line Generation in extractLinesCanonical ⚠️ MEDIUM CONFIDENCE

**Evidence:**
- `BidirectionalScanner.lhs:469-470` combines partial lines
- If both `partial` and `edgePiece` are empty → empty Text
- Empty text gets cached by offset and returned to viewport

**Trigger:**
- Initial reads (1-25): All pieces have content
- Scroll 1-2: Normal operation
- Scroll 3+: Start hitting edge boundaries with empty pieces

#### HYPOTHESIS 4: Off-by-One in Boundary Check ⚠️ MEDIUM CONFIDENCE

**Evidence:**
- `Operations.hs:111` checks `firstLineNum <= 1`
- Uses viewport's firstLineNum, not cursor tracking
- Corrupted line numbers make boundary check unreliable
- Stops at line 15 when checking becomes false

---

### Agent 2 (Task): Debug Output Analysis

**Critical Discovery:**

**Empty lines appear at alternating positions: 27, 29, 31, 33...**

**Pattern at Line 30 viewport:**
```
Line 26: len=243  (correct)
Line 27: len=0    (EMPTY - should be 223 chars)
Line 28: len=243  (correct)
Line 29: len=0    (EMPTY - should be 224 chars)
Line 30: len=223  (correct but SHIFTED)
```

**Evidence:**
- ✓ Direct cache reads (`getLinesFromStart`) return correct data
- ✗ Scroll operations (`scrollDown` + `getLinesFrom`) return corrupted data
- ✗ Every other new line added during scroll is empty
- ✗ Suggests byte offset calculation is wrong

**Files Generated:**
- 11 debug documents in `C:\GitHub\HaFileViewer.Stack\`
- `00_START_HERE.md` - Investigation overview
- `VIEWPORT_DEBUG_REPORT.md` - Complete analysis (8.5 KB)
- `DEBUG_VISUAL_SUMMARY.txt` - Visual comparison
- `debug_viewport_output.txt` - Raw debug output

---

## Root Cause Conclusion

### Primary Suspect: Line Number Calculation in Backward Direction

**Location:** `src/HaFileViewer/LineCache.lhs` lines 406-408

**The Bug:**
```haskell
lineNumbers = case dir of
  Forward -> [startLineNum .. startLineNum + fromIntegral count - 1]
  Backward -> reverse [startLineNum - fromIntegral count + 1 .. startLineNum]
```

**Why it's wrong:**
1. `BidirectionalScanner` returns lines in **file order** (top to bottom) regardless of direction
2. For Backward scans, the scanner reads backwards but returns them forwards
3. The `reverse` operation creates a **mismatch** between line order and line numbers
4. When zipped together (line 347), lines get wrong numbers

**Example:**
```haskell
-- Reading 3 lines backward from line 30 (startLineNum=30)
Scanner returns: ["line 28", "line 29", "line 30"]  -- file order
Line numbers:    [28, 29, 30]                       -- after reverse

SHOULD BE:       [30, 29, 28]  -- to match reverse scan
```

### Secondary Issue: Accumulation During Multi-Directional Scrolling

**Location:** `src/HaFileViewer/CUILogViewer/Operations.hs`

**The Problem:**
- `scrollDown` increments: `cursorFirstLine cursor + 1`
- `scrollUp` decrements: `cursorFirstLine cursor - 1`
- If cache returns wrong line numbers, cursor tracking drifts
- After 76 downs + 61 ups, drift accumulates to 14 lines

**Why alternating empty lines:**
- Wrong line numbers cause cache misses
- Cache returns empty default for missing keys
- Every other scroll hits a miss → empty line
- Pattern: hit, miss, hit, miss, hit, miss...

---

## Validation Evidence

### Direct Cache Read (Correct):
```
getLinesFromStart cache 30
Returns: 30 lines, all with content, correct line numbers
```

### Scroll Down (Corrupted):
```
Initial: lines 1-25 (correct)
Scroll 3: lines 3-27 (line 27 is empty) ✗
Scroll 4: lines 4-28 (line 28 OK, line 27 still empty) ✗
Scroll 5: lines 5-29 (line 29 is empty) ✗
```

### Scroll Up Stop Early:
```
From line 76-100
Scroll up 61 times
Expected: line 1-25
Actual: line 15-39 (stopped 14 lines short) ✗
```

---

## Next Steps

### Fix Strategy:

1. **Fix line number calculation in LineCache.lhs (lines 406-408)**
   - Remove `reverse` or adjust scanner return order
   - Ensure line numbers match actual line positions

2. **Verify cursor tracking in Operations.hs**
   - Ensure `cursorFirstLine`/`cursorLastLine` use corrected line numbers
   - Add assertions to catch drift

3. **Add regression test**
   - Use `test_bug_scroll_regression.hs`
   - Verify no empty lines after 76 scrolls
   - Verify scroll-up returns to line 1

### Verification:
- Run `test_bug_scroll_regression.hs` after fix
- Should show: No empty lines, stops at line 1
- All 20 systematic tests should still pass

---

## Investigation Files

**Location:** `C:\GitHub\HaFileViewer.Stack\`

**Start here:**
1. `00_START_HERE.md` - Overview
2. `VIEWPORT_DEBUG_REPORT.md` - Complete findings
3. `DEBUG_VISUAL_SUMMARY.txt` - Side-by-side comparison
4. `debug_viewport_output.txt` - Raw evidence

**This document:** `BUG_INVESTIGATION_RESULTS.md`

---

**Status:** ✅ Root cause identified, ready for fix implementation
