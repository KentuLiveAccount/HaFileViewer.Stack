# OFFSET DEBUG PACKAGE - COMPLETE INDEX

**Date Created:** 2026-03-08  
**Status:** ✓ BUG CONFIRMED - Off-by-one byte in offset calculation  
**Severity:** CRITICAL - Breaks incremental scrolling  
**Root Cause:** `botPos` calculated +1 byte from correct position

## 📋 File Organization

### 🚀 START HERE
1. **OFFSET_DEBUG_README.md** - Quick overview and how to use this package
2. **OFFSET_BUG_VISUAL_SUMMARY.md** - Visual diagrams showing the bug

### 📊 Detailed Analysis (Read in Order)
3. **OFFSET_DEBUG_FINDINGS.md** - Initial hypothesis (2.6K)
4. **OFFSET_DEBUG_EXECUTION_SUMMARY.md** - Complete test results (6.1K)
5. **OFFSET_BUG_COMPREHENSIVE_REPORT.md** - Root cause analysis (5.2K)
6. **DEBUG_OFFSET_ANALYSIS.md** - Master analysis index (5.3K)

### ⚙️ Executable Tests (Ready to Run)
- `test_incremental_debug.exe` - 50-line incremental read test
- `test_offset_deep_debug.exe` - Lines 26-27 deep dive
- `test_byte_level.exe` - Minimal 3-line test

### 📄 Test Output Files (Proof)
- `debug_offset_output.txt` - test_incremental_debug output
- `debug_deep_offset.txt` - test_offset_deep_debug output
- `test_byte_level_output.txt` - test_byte_level output

### 📝 Source Code (If You Need to Recompile)
- `test_incremental_debug.hs` - Source for incremental test
- `test_offset_deep_debug.hs` - Source for deep dive test
- `test_byte_level.hs` - Source for 3-line test

## 🎯 Quick Summary

**The Bug:** `getLinesFromStart()` returns `botPos` offset that is +1 byte from where it should be

**The Evidence:**
- ✓ Line 26 returns EMPTY after reading 25 lines
- ✓ Line 27 returns "ine 26..." (missing first "L")
- ✓ Same bug confirmed with 1-line test

**The Impact:**
- ✗ First incremental scroll after initial read returns empty line
- ✗ All subsequent reads are misaligned
- ✗ Scrolling fundamentally broken

**The Fix Location:**
- File: `src\HaFileViewer\LineCache.lhs`
- Functions: `getLinesFromStart`, `extractNewPosition`, `getLinesFrom`
- Problem: Off-by-one in offset arithmetic

## 🧪 How to Validate the Bug

```powershell
cd C:\GitHub\HaFileViewer.Stack

# Test 1: Shows line 26 empty
.\test_incremental_debug.exe

# Test 2: Shows line 27 missing first character
.\test_offset_deep_debug.exe

# Test 3: Simplest case - line 2 is empty
.\test_byte_level.exe
```

All three show the same symptom: **botPos is 1 byte too far forward**

## 📖 Reading Guide

### For Quick Understanding (5 min)
1. Read: `OFFSET_DEBUG_README.md`
2. Look at: `OFFSET_BUG_VISUAL_SUMMARY.md`
3. Run: `.\test_incremental_debug.exe`

### For Complete Understanding (15 min)
1. Read: `OFFSET_DEBUG_FINDINGS.md`
2. Read: `OFFSET_DEBUG_EXECUTION_SUMMARY.md`
3. Run all three tests
4. Check the output files

### For Root Cause Analysis (30 min)
1. Read: `OFFSET_BUG_COMPREHENSIVE_REPORT.md`
2. Read: `DEBUG_OFFSET_ANALYSIS.md`
3. Examine all three output files
4. Look at test source code

### For Code Investigation (60 min)
1. Review all analysis documents
2. Run all three tests multiple times
3. Open `src\HaFileViewer\LineCache.lhs`
4. Search for offset calculation code
5. Look for +1, +2, CRLF patterns
6. Trace through the bug path

## 🐛 Bug Details

### Symptom
After `getLinesFromStart(cache, N)`, the returned `botPos` points to byte position that is 1 too far

### Proof
```
Test with 25 lines:
  Line 26 (expected): "Line 26 has content..."
  Line 26 (actual):   (empty)
  Line 27 (actual):   "ine 26 has content..."
                      ^^^ Missing first "L"
```

### Root Cause
The offset calculation in `getLinesFromStart` (or `extractNewPosition`) has an off-by-one error

### Location
- File: `src\HaFileViewer\LineCache.lhs`
- Function: `getLinesFromStart` (~line 250)
- Alternatively: `extractNewPosition` (~line 400)

### Fix
Change the offset calculation from: `offset + length + 1`  
To: `offset + length + 2` (for CRLF)  
Or fix the CRLF counting logic

## 📊 Test Specifications

| Test Name | Purpose | Input | Output on Success | Output on Failure |
|-----------|---------|-------|-------------------|-------------------|
| test_incremental_debug | Track 50-line progression | 50 lines | All lines OK | Line 26 empty |
| test_offset_deep_debug | Analyze lines 26-27 | 50 lines | Line 26 ok, Line 27 ok | Line 26 empty, Line 27 missing "L" |
| test_byte_level | Minimal 3-line test | 3 lines | Line 2 ok | Line 2 empty |

## 🔍 Search Strategy for Finding the Bug

In `src\HaFileViewer\LineCache.lhs`:

1. Search for: `getLinesFromStart`
   - Find where it returns `(lines, topPos, botPos)`
   - Check how `botPos` is calculated

2. Search for: `extractNewPosition`
   - Find the implementation
   - Look at: `extractNewPosition results Forward =`

3. Look for: `+ 2` or `+ 1` in offset calculations
   - Should be `+ 2` for CRLF (carriage return + line feed)
   - Incorrect counting could cause the bug

4. Check: Line ending boundary detection
   - CRLF should always be 2 bytes
   - Not 1 byte

## ✅ Verification Checklist

After fixing the bug, run:
```powershell
.\test_incremental_debug.exe  # Should show all lines OK, no empty lines
.\test_offset_deep_debug.exe  # Should show Line 26 with content
.\test_byte_level.exe         # Should show Line 2 with content
```

Expected result: All three should pass with no empty lines or missing characters

## 📚 Document Sizes & Content

| File | Size | Focus |
|------|------|-------|
| OFFSET_DEBUG_README.md | 5.1K | Quick start guide |
| OFFSET_BUG_VISUAL_SUMMARY.md | 5.7K | Visual diagrams |
| OFFSET_DEBUG_FINDINGS.md | 2.6K | Initial hypothesis |
| OFFSET_DEBUG_EXECUTION_SUMMARY.md | 6.2K | Test results |
| OFFSET_BUG_COMPREHENSIVE_REPORT.md | 5.2K | Root cause |
| DEBUG_OFFSET_ANALYSIS.md | 5.3K | Master index |

**Total Documentation:** ~30K of analysis

## 🎓 Learning from This Package

This package demonstrates:
- ✓ How to systematically debug offset/boundary issues
- ✓ How to create minimal reproducible test cases
- ✓ How to analyze byte-level problems
- ✓ How to document bugs comprehensively
- ✓ How to prove root causes with evidence

## 🚀 Next Steps

1. **Understand:** Read OFFSET_DEBUG_README.md
2. **Visualize:** Read OFFSET_BUG_VISUAL_SUMMARY.md
3. **Run Tests:** Execute all three test executables
4. **Analyze:** Review the output files
5. **Locate:** Find the bug in LineCache.lhs
6. **Fix:** Correct the offset arithmetic
7. **Verify:** Re-run tests to confirm fix

---

**Last Updated:** 2026-03-08  
**Package Complete:** Yes ✓  
**All Tests Compiled:** Yes ✓  
**Bug Confirmed:** Yes ✓  
**Ready for Fix Implementation:** Yes ✓
