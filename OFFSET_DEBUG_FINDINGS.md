# Offset Debug Findings

## Problem Summary

The incremental scroll test reveals a critical offset calculation bug:

### Symptom
- First incremental read (line 26) returns **empty line**
- Second incremental read (line 27) returns text missing the first character: `'ine 26 has content...'` instead of `'Line 26 has content...'`

### Root Cause
The `botPos1` offset returned by `getLinesFromStart(cache, 25)` is **off by 1 byte**.

### Evidence

From test_offset_deep_debug.exe output:

```
Step 1: Read lines 1-25
  Got 25 lines
  botPos1 origin: FromStart

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
                  ^^^ MISSING THE "L" CHARACTER!
    Is empty: False
```

### Analysis

The botPos1 must be pointing **1 byte into line 26** instead of:
- **Before** line 26, OR
- **After** the "L" but before "ine"

When we then read from botPos26 (after the empty line), we get "ine 26..." which proves the offset was off.

### File Structure

Each line is formatted as:
```
"Line N has content here to make it non-empty\r\n"
```

Counting characters for a typical line:
- "Line 26 has content here to make it non-empty" = 44 chars (confirmed by step 3 output)
- Plus CR-LF = 2 chars
- **Total per line = 46 chars**

So the offset should advance by 46 bytes for each line.

### Where the Bug Occurs

Looking at line 27's output: `'ine 26 has content here to make it non-empty'` = 44 chars

This is the 44 chars MINUS the first "L", suggesting the offset is pointing at byte position 1 (or offset by 1 from where it should be).

### Next Steps

**Check in `LineCache.lhs`:**
1. How `getLinesFromStart` calculates `botPos1`
2. The `extractNewPosition` function that computes the next offset
3. Off-by-one errors in line ending detection (CR-LF vs LF)
4. Offset calculation in `getLinesFrom` when starting from a position

**Most likely location:** The calculation in `getLinesFromStart` around line 250-260 that determines where line 25 ends.

**Hypothesis:** The code may be:
- Returning the offset of the first character of line 26 instead of the last character of line 25
- OR miscalculating the CRLF boundary (counting 1 instead of 2 for line ending)
- OR the initial `botPos` is being calculated as the start of line 26 rather than the end of line 25
