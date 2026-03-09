# OFFSET DEBUG - Master Analysis Document

**Date:** 2026-03-08  
**Status:** BUG CONFIRMED - Off-by-one byte in offset calculation  
**Severity:** CRITICAL - Breaks incremental scrolling completely

## Quick Summary

The `botPos` offset returned by `getLinesFromStart()` is **off by 1 byte**, causing:
- Line 26 returns empty after reading 25 lines
- Line 27 loses its first character (e.g., "ine 26..." instead of "Line 26...")
- All incremental reads fail

## Created Debug Tests

### 1. test_incremental_debug.exe
**Purpose:** Identify where alternating pattern starts  
**Input:** 50-line test file  
**Key Output:**
```
Line 26  | FAIL: EMPTY
Line 27  | OK (44 chars)
Line 28  | OK (45 chars)
...
```
**Finding:** First incremental read returns empty line  
**Output File:** `debug_offset_output.txt`

### 2. test_offset_deep_debug.exe
**Purpose:** Show exact content mismatch  
**Input:** 50-line test file  
**Key Output:**
```
Step 2: Read line 26 from botPos1
  Text length: 0 chars
  Text content: ''
  Is empty: True

Step 3: Read line 27 from botPos26
  Text length: 44 chars
  Text content: 'ine 26 has content here to make it non-empty'
                ^^^ MISSING FIRST CHARACTER
```
**Finding:** botPos points 1 byte INTO line 26, not at its start  
**Output File:** `debug_deep_offset.txt`

### 3. test_byte_level.exe
**Purpose:** Simplest case - 3 lines only  
**Input:** Minimal test file with 3 lines (48 bytes total)  
**Key Output:**
```
getLinesFromStart cache 1:
  Content: 'Line 1 content'

Continue from bot1:
  Line 2: ''
  (EMPTY - offset is wrong!)
```
**Finding:** After reading 1 line, botPos doesn't point to line 2  
**Output File:** `test_byte_level_output.txt`

## Root Cause Analysis

### The Bug

The offset calculation in `LineCache.lhs` has an off-by-one error:
- `botPos` should point to the **first byte of the next line** (or EOF)
- `botPos` currently points to **one byte past that** (or into line content)

### Probable Location

In `LineCache.lhs`, check:

1. **`getLinesFromStart` function** (~line 250-260)
   - How it constructs the returned `botPos`
   - The calculation should be: offset of last line + length of last line + 2 (for CRLF)
   - Currently might be: ...+ 1 or off by CRLF handling

2. **`extractNewPosition` function** (~line 400-410)
   - Calculates next offset from line results
   - Used by `getLinesFrom` to find where to read next
   - Off-by-one in line ending boundary detection

### Evidence Chain

**File content:** `"Line 26 has content here to make it non-empty\r\n"`
- Content: 44 characters
- Line ending: 2 bytes (CR-LF)
- **Total per line: 46 bytes**

**When line 27 is read**, result shows: `"ine 26 has content here to make it non-empty"` (44 chars)
- This is the ORIGINAL line 26 content MINUS the first "L"
- Proves offset is +1 byte into line 26
- The botPos from line 26's failed read skips 1 more byte

**When continuing further**, the pattern continues correctly because each read advances by the amount it read, not by the intended line length.

## How to Fix

1. **Open:** `src\HaFileViewer\LineCache.lhs`

2. **Find:** The definition of `botPos` in `getLinesFromStart`
   - Look for the return statement that constructs `botPos` or `extractNewPosition`

3. **Check:** How the offset is calculated after reading N lines
   - Should be: `startOffset + sum(lineLength + 2 for each line)`
   - Verify the CRLF (2 bytes) is counted correctly, not as 1 byte

4. **Fix:** Adjust by -1 byte or fix the CRLF counting logic

5. **Test:** Run the three debug tests to verify:
   ```powershell
   .\test_incremental_debug.exe     # Should pass all 50 lines
   .\test_offset_deep_debug.exe     # Should get correct line 26
   .\test_byte_level.exe            # Line 2 should not be empty
   ```

## All Debug Output Files

| File | Size | Test | Key Finding |
|------|------|------|-------------|
| `debug_offset_output.txt` | 2.3K | test_incremental_debug | Line 26 empty |
| `debug_deep_offset.txt` | 1.3K | test_offset_deep_debug | Line 27 missing "L" |
| `test_byte_level_output.txt` | 1.0K | test_byte_level | After 1 line, next is empty |
| `OFFSET_DEBUG_FINDINGS.md` | 2.7K | Analysis | Hypothesis about -1 byte |
| `OFFSET_BUG_COMPREHENSIVE_REPORT.md` | 5.4K | Full Report | Complete root cause analysis |

## Next Steps

1. Review the evidence in each test output
2. Locate the offset calculation code in `LineCache.lhs`
3. Fix the off-by-one error
4. Re-run all three debug tests to confirm fix
5. Run full test suite to ensure no regression

## Key Files Modified/Created

**Debug Tests Created:**
- `test_incremental_debug.hs` - 50-line test with incremental reads
- `test_offset_deep_debug.hs` - Deep dive into lines 26-27
- `test_byte_level.hs` - Minimal 3-line test

**Analysis Documents Created:**
- `OFFSET_DEBUG_FINDINGS.md` - Initial hypothesis
- `OFFSET_BUG_COMPREHENSIVE_REPORT.md` - Full root cause analysis
- `DEBUG_OFFSET_ANALYSIS.md` - This master document

**Output Files Generated:**
- `debug_offset_output.txt` - test_incremental_debug output
- `debug_deep_offset.txt` - test_offset_deep_debug output  
- `test_byte_level_output.txt` - test_byte_level output
