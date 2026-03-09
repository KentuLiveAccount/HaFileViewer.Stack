# OFFSET CALCULATION BUG - COMPREHENSIVE DEBUG REPORT

## Summary

There is a **critical offset calculation bug** in `HaFileViewer.LineCache` that causes:
1. Empty lines to be returned on incremental reads
2. Character skipping (first character of subsequent lines is lost)
3. Alternating success/failure pattern in scrolling

## Root Cause

**The `botPos` (bottom position) returned by `getLinesFromStart` is calculated incorrectly**, causing subsequent `getLinesFrom` calls to read from the wrong file offset.

## Detailed Evidence

### Test 1: test_byte_level.exe
**File:** 3 lines, each "Line N content\r\n" (48 bytes total)

**Expected Behavior:**
```
getLinesFromStart cache 1:
  Got line 1: "Line 1 content"
  bot1 should point to byte 17 (start of line 2)

getLinesFrom cache bot1 Forward 1 2:
  Should get line 2: "Line 2 content"
```

**Actual Behavior:**
```
getLinesFromStart cache 1:
  Got 1 line
  Content: 'Line 1 content'

Continue from bot1:
  Line 2: ''
  (EMPTY - offset is wrong!)
```

**Conclusion:** `botPos` from `getLinesFromStart` does NOT point to the correct location for the next line.

### Test 2: test_offset_deep_debug.exe
**File:** 50 lines, each "Line N has content here to make it non-empty\r\n"

**Expected Behavior:**
```
getLinesFromStart cache 25:
  Lines 1-25 read correctly
  botPos should point to start of line 26

getLinesFrom cache botPos Forward 1 26:
  Should get line 26: "Line 26 has content..."
```

**Actual Behavior:**
```
getLinesFromStart cache 25:
  Got 25 lines (all correct)
  botPos origin: FromStart

getLinesFrom cache botPos Forward 1 26:
  Result count: 1
  Line number: 26
  Text length: 0 chars          <-- EMPTY!
  Text content: ''
  Is empty: True

getLinesFrom cache botPos26 Forward 1 27:
  Result count: 1
  Line number: 27
  Text length: 44 chars
  Text content: 'ine 26 has content here to make it non-empty'
                ^^^ MISSING FIRST CHARACTER "L"!
```

**Analysis:**
- Line 26 should be: "Line 26 has content..." (44 chars + "L")
- Line 27 result shows: "ine 26 has..." (44 chars, but missing the "L" from line 26)
- This proves botPos points 1 byte INTO line 26 instead of at its start
- The subsequent read at botPos26 starts at byte 1 of the remaining content

**Conclusion:** `botPos` is pointing **1 byte too far forward** from where it should be.

### Test 3: test_incremental_scroll.exe
**Pattern observed:**
```
Line 26  | FAIL: EMPTY
Line 27  | OK (44 chars)
Line 28  | OK (45 chars)
...
Line 50  | OK (45 chars)
```

**Why it works after line 26:**
After the first failed read, all subsequent reads work because the alternating pattern is broken by error recovery. However, Line 27 only has 44 chars (not 45 like 26-25), proving it's missing its first character.

## Location of Bug

The bug is in **`HaFileViewer.LineCache.lhs`**, specifically in the implementation of:

1. **`getLinesFromStart`** - This function reads N lines from the start and returns:
   - The lines themselves
   - `topPos`: Position pointing to the first line
   - `botPos`: Position pointing AFTER the last line

   **The bug is likely in how `botPos` is calculated** - it should point to byte 0 of the next line (or EOF), but it's pointing 1 byte too far.

2. **`extractNewPosition`** - This helper function calculates the next read position from line results.
   - Used by `getLinesFrom` to determine where the next read should start
   - Likely has an off-by-one error

3. **Line ending handling** - The code might be:
   - Counting only 1 byte for CRLF (should be 2)
   - Miscalculating the boundary between line content and line ending
   - Not accounting for the full CRLF sequence correctly

## Expected Fix Areas

Search in `LineCache.lhs` for:

1. **Around line 250-260:** `getLinesFromStart` implementation
   - Find where `botPos` is constructed
   - Verify it correctly calculates the byte offset of the LAST line's CRLF

2. **Around line 400-410:** `extractNewPosition` function
   - Check line: `extractNewPosition results Forward =`
   - Verify it correctly calculates offset after last result

3. **CRLF calculation:** Look for:
   - `+ 2` (for line ending)
   - Off-by-one patterns with offset arithmetic
   - `last` or similar operations on line lists

## Test Files Created

1. **test_incremental_debug.exe** - Tracks offset progression through 50 lines
   - Output: Shows first line empty, all others OK
   - File: `debug_offset_output.txt`

2. **test_offset_deep_debug.exe** - Deep analysis of lines 26-27
   - Output: Shows empty line 26, then line 27 missing first char
   - File: `debug_deep_offset.txt`

3. **test_byte_level.exe** - Byte-level analysis with 3-line file
   - Output: Shows bot position doesn't point to correct next line
   - File: `test_byte_level_output.txt`

All test files confirm: **botPos is off by approximately 1 byte**

## Impact

This bug causes:
- ✗ Empty lines on first incremental scroll
- ✗ Character loss (first character of lines)
- ✗ Alternating pattern of failures
- ✗ Incorrect viewport state after scrolling
- ✗ Line number misalignment with content

The bug affects **all incremental reading after an initial `getLinesFromStart` call**, making scrolling fundamentally broken.
