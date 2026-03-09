# OFFSET DEBUG EXECUTION SUMMARY

**Created:** 2026-03-08  
**Status:** Three independent debug tests confirm identical off-by-one bug

## Test Execution Results

### Test 1: test_incremental_debug.exe
**Command:** `.\test_incremental_debug.exe`  
**Purpose:** Track offset progression through 50 incremental reads  
**Result:** ✗ FAILED

```
Line 26  | Input Pos: 26 | Output Pos: 26 | Lines read: 1 | FAIL: EMPTY
Line 27  | Input Pos: 27 | Output Pos: 27 | Lines read: 1 | OK (44 chars)
Line 28  | Input Pos: 28 | Output Pos: 28 | Lines read: 1 | OK (45 chars)
...
Line 50  | Input Pos: 50 | Output Pos: 50 | Lines read: 1 | OK (45 chars)

=== Results ===
FAILURE: Found 1 problems:
  - Line 26: Empty
```

**Key Finding:** First incremental read after `getLinesFromStart(25)` returns empty line for line 26

---

### Test 2: test_offset_deep_debug.exe
**Command:** `.\test_offset_deep_debug.exe`  
**Purpose:** Deep dive into why line 26 is empty and line 27 is truncated  
**Result:** ✗ FAILED

```
Step 2: Read line 26 from botPos1
  getLinesFrom returned:
    Result count: 1
    Line number: 26
    Text length: 0 chars      <-- EMPTY!
    Text content: ''
    Is empty: True

Step 3: Read line 27 from botPos26
  getLinesFrom returned:
    Result count: 1
    Line number: 27
    Text length: 44 chars
    Text content: 'ine 26 has content here to make it non-empty'
                  ^^^ MISSING FIRST CHARACTER "L"!
    Is empty: False
```

**Key Finding:** 
- Line 26 is empty (0 chars)
- Line 27 shows "ine 26..." instead of "Line 26..." (missing first "L")
- This proves botPos points 1 byte INTO line 26, not at its start
- The subsequent read from botPos26 is therefore also at +1 offset

---

### Test 3: test_byte_level.exe
**Command:** `.\test_byte_level.exe`  
**Purpose:** Simplest possible case - 3-line file to eliminate variables  
**Result:** ✗ FAILED

```
File size: 48 bytes

Expected line boundaries:
  Line 1: bytes 0-14 (15 bytes for 'Line 1 content')
  CRLF:   bytes 15-16 (2 bytes for \r\n)
  Line 2: bytes 17-31 (15 bytes for 'Line 2 content')
  CRLF:   bytes 32-33
  Line 3: bytes 34-48 (15 bytes for 'Line 3 content')
  CRLF:   bytes 49-50

Step 4: Read just line 1, then continue
getLinesFromStart cache 1:
  Got 1 line
  Content: 'Line 1 content'

Continue from bot1:
  Line 2: ''
  (EMPTY - offset is wrong!)
```

**Key Finding:**
- After reading 1 line from start, botPos should point to byte 17 (start of line 2)
- Reading from botPos returns EMPTY line
- This confirms botPos is not pointing to the correct location
- The bug is in `getLinesFromStart` or `extractNewPosition`

---

## Bug Confirmation Matrix

| Test | Input | Expected | Actual | Status |
|------|-------|----------|--------|--------|
| test_incremental_debug | 25 lines | Line 26 has content | Line 26 is empty | ✗ FAIL |
| test_offset_deep_debug | 25 lines | Read line 26 normal | Line 26 empty, line 27 missing "L" | ✗ FAIL |
| test_byte_level | 1 line | Read line 2 next | Line 2 is empty | ✗ FAIL |

**Consistency:** All three tests show the SAME symptom: `botPos` is off by 1 byte

---

## Root Cause Identified

The offset calculation bug is a **+1 byte error** in how `botPos` is computed:

### Current (Buggy) Behavior
```
getLinesFromStart(N lines) returns:
  botPos = offset that is 1 byte PAST where next line actually starts
```

### Expected Behavior
```
getLinesFromStart(N lines) should return:
  botPos = offset pointing to first byte of (N+1)th line (or EOF)
```

### The Off-by-One Error

When reading N lines:
1. Each line is: `content\r\n`
2. botPos should be: `startOffset + sum(lineLength + 2 for each line)`
3. Currently it seems to be: `startOffset + sum(lineLength + 2 for each line) + 1`

OR more likely:

1. The CRLF is being counted as 1 byte instead of 2
2. Or there's an off-by-one in the line ending boundary detection

---

## Debug Test Files

All three test executables have been compiled and are ready to run:

```
test_incremental_debug.exe  (4.7K)  - Read 50 lines incrementally
test_offset_deep_debug.exe  (5.2K)  - Deep dive on lines 26-27
test_byte_level.exe         (4.8K)  - Minimal 3-line test
```

**To re-run any test:**
```powershell
cd C:\GitHub\HaFileViewer.Stack
.\test_incremental_debug.exe   # Shows line 26 empty
.\test_offset_deep_debug.exe   # Shows line 27 missing "L"
.\test_byte_level.exe          # Shows line 2 empty
```

---

## Source Code Location

**File to Fix:** `src\HaFileViewer\LineCache.lhs`

**Functions to Investigate:**
1. `getLinesFromStart` (around line 250-260)
   - How it calculates the returned `botPos`

2. `extractNewPosition` (around line 400-410)
   - How it calculates next offset from line results

3. `getLinesFrom` (around line 300-330)
   - How it uses positions for subsequent reads

**Search for these patterns:**
```haskell
botPos <- ... extractNewPosition ...
extractNewPosition results Forward = 
extractNewPosition results Backward =
+ 2  -- for CRLF
```

---

## Summary of Evidence

1. ✓ Test 1 confirms: First incremental read returns empty
2. ✓ Test 2 confirms: Empty line + truncated next line = +1 byte offset
3. ✓ Test 3 confirms: Even with 1-line input, next line is empty

**All three tests point to identical bug:** botPos is off by 1 byte

The bug is in the offset calculation within `LineCache.lhs`, specifically in how the bottom/next position is determined after reading lines.

---

## Files Generated

**Output Files:**
- `debug_offset_output.txt` - test_incremental_debug output
- `debug_deep_offset.txt` - test_offset_deep_debug output
- `test_byte_level_output.txt` - test_byte_level output

**Analysis Documents:**
- `OFFSET_DEBUG_FINDINGS.md` - Initial hypothesis
- `OFFSET_BUG_COMPREHENSIVE_REPORT.md` - Root cause analysis
- `DEBUG_OFFSET_ANALYSIS.md` - Master analysis index
- `OFFSET_DEBUG_EXECUTION_SUMMARY.md` - This document

**Test Source Files:**
- `test_incremental_debug.hs` - 50-line incremental test
- `test_offset_deep_debug.hs` - Deep dive test
- `test_byte_level.hs` - 3-line minimal test
