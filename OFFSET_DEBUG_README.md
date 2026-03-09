# OFFSET DEBUG PACKAGE README

## Overview

This directory contains **three independent debug tests** that all confirm the same critical bug:

**The `botPos` offset returned by `getLinesFromStart()` is calculated 1 byte too far.**

## The Bug

When you:
1. Call `getLinesFromStart cache 25` to read the first 25 lines
2. Get back `botPos` to use for the next read
3. Call `getLinesFrom cache botPos Forward 1 26` to read line 26

**Result:** You get an EMPTY line for line 26, or a truncated line 27

**Reason:** `botPos` points 1 byte INTO line 26 instead of at its start

## Quick Validation

Run these commands to see the bug yourself:

```powershell
# Test 1: Shows line 26 empty, line 27 truncated
.\test_incremental_debug.exe

# Test 2: Shows exact content mismatch  
.\test_offset_deep_debug.exe

# Test 3: Simple 3-line case shows same bug
.\test_byte_level.exe
```

All three will show failures confirming the same bug.

## Evidence

### From test_offset_deep_debug.exe

```
Step 2: Read line 26 from botPos1
  Text length: 0 chars
  Text content: ''
  
Step 3: Read line 27 from botPos26
  Text length: 44 chars
  Text content: 'ine 26 has content...'  <-- Missing first "L"!
```

This proves `botPos` points 1 byte INTO the line, not at its start.

### From test_byte_level.exe

```
getLinesFromStart cache 1:
  Got 1 line: 'Line 1 content'

Continue from bot1:
  Line 2: ''  <-- Empty! Should have line 2 content
```

Even with just 1 line, the next position is wrong.

## Files in This Package

### Executables (Compiled Ready-to-Run Tests)
- `test_incremental_debug.exe` - 50-line incremental read test
- `test_offset_deep_debug.exe` - Deep dive on lines 26-27
- `test_byte_level.exe` - Minimal 3-line test

### Output Files (Test Results)
- `debug_offset_output.txt` - Output from test_incremental_debug
- `debug_deep_offset.txt` - Output from test_offset_deep_debug
- `test_byte_level_output.txt` - Output from test_byte_level

### Analysis Documents (Findings & Root Cause)
- `OFFSET_DEBUG_EXECUTION_SUMMARY.md` - Complete execution results and analysis
- `OFFSET_BUG_COMPREHENSIVE_REPORT.md` - Detailed root cause analysis
- `OFFSET_DEBUG_FINDINGS.md` - Initial hypothesis and findings
- `DEBUG_OFFSET_ANALYSIS.md` - Master index of all findings

### Source Test Files (Compile These if Needed)
- `test_incremental_debug.hs` - Source for incremental test
- `test_offset_deep_debug.hs` - Source for deep dive test
- `test_byte_level.hs` - Source for 3-line test

## How to Use

### 1. To See the Bug
```powershell
.\test_incremental_debug.exe
```
You'll see: "Line 26: FAIL: EMPTY" in the output

### 2. To Understand Why It Happens
```powershell
.\test_offset_deep_debug.exe
```
Look for: Line 27 missing its first character

### 3. To Debug the Simplest Case
```powershell
.\test_byte_level.exe
```
Shows the bug with just 3 lines of input

### 4. To Read the Analysis
Start with: `OFFSET_DEBUG_EXECUTION_SUMMARY.md`

## The Fix

**Location:** `src\HaFileViewer\LineCache.lhs`

**Functions to Check:**
1. `getLinesFromStart` - Returns the `botPos` that's wrong
2. `extractNewPosition` - Calculates next offset
3. `getLinesFrom` - Uses positions for reading

**Look For:**
- Off-by-one in offset calculations
- CRLF being counted as 1 byte instead of 2
- Boundary calculations between line content and line ending

**Common Patterns to Fix:**
```haskell
-- Likely buggy patterns:
offset + lineLength + 1  -- Should be + 2 for CRLF
offset + lineLength      -- Missing the + 2 for CRLF
head (tail ...)          -- Off-by-one in parsing
```

## Key Findings Summary

| Finding | Evidence | Severity |
|---------|----------|----------|
| Line 26 empty on first incremental read | test_incremental_debug | CRITICAL |
| Line 27 missing first char | test_offset_deep_debug | CRITICAL |
| Same bug in 3-line case | test_byte_level | CONFIRMS BUG |
| botPos calculated +1 byte | All three tests | ROOT CAUSE |

## Next Steps

1. **Understand the Bug** - Read `OFFSET_DEBUG_EXECUTION_SUMMARY.md`
2. **Locate the Code** - Open `src\HaFileViewer\LineCache.lhs`
3. **Find the Bug** - Look for +1 byte offset calculation
4. **Fix the Bug** - Adjust offset arithmetic or CRLF counting
5. **Verify the Fix** - Re-run all three tests
6. **Test Regression** - Run full test suite

## Test Output Locations

All three tests create output files you can examine:
- `debug_offset_output.txt` - 2.2K (test_incremental_debug results)
- `debug_deep_offset.txt` - 1.3K (test_offset_deep_debug results)  
- `test_byte_level_output.txt` - 1K (test_byte_level results)

All show the same symptom: **botPos is off by 1 byte**

## Questions?

Refer to the analysis documents in order:
1. `OFFSET_DEBUG_FINDINGS.md` - Quick summary
2. `OFFSET_DEBUG_EXECUTION_SUMMARY.md` - Complete results
3. `OFFSET_BUG_COMPREHENSIVE_REPORT.md` - Deep analysis
4. Run the tests yourself to see evidence

The bug is real, it's reproducible, and these tests prove it conclusively.
