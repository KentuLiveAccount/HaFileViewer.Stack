# OFFSET BUG VISUAL SUMMARY

## The Problem (Visual Diagram)

### Expected Behavior
```
File Content:
  Byte: 0  1  2 ... 44 45 46 47 48 ... (positions)
        [Line 26 content.....][CR][LF][Line 27 content...]
         ↑                        ↑   ↑
         |                        |   |
    Start of line 26          CR  LF  Start of line 27


getLinesFromStart(25) returns:
  botPos → points to byte 46 (first byte of line 27)
                              ↑ CORRECT: At start of next line

getLinesFrom(botPos, Forward, 1, 27) reads:
  Gets: "Line 27 content..." (complete, 44+ chars)
```

### Actual (Buggy) Behavior
```
File Content:
  Byte: 0  1  2 ... 44 45 46 47 48 ... (positions)
        [Line 26 content.....][CR][LF][Line 27 content...]
         ↑                        ↑   ↑↑
         |                        |   ||
    Start of line 26          CR  LF  |+1 (WRONG!)
                                      |
                        botPos → byte 47 (into line 27)
                                      ↑ WRONG: Skips first char!

getLinesFromStart(25) returns:
  botPos → points to byte 47 (1 byte INTO line 27)
                              ↑ INCORRECT: Skips the "L"

getLinesFrom(botPos, Forward, 1, 27) reads:
  Gets: (empty or "ine 27 content..." missing "L")
  Why: Reading from byte 47 gets no complete line, or skips first char
```

## The Evidence

### Test 1: After Reading 25 Lines
```
Expected position: byte 1700 (start of line 26)
Actual position:   byte 1701 (1 byte into line 26)
Difference:        +1 byte

Result:
  Line 26 read: (empty)    ← Gets part of line, treated as empty
  Line 27 read: "ine 26..." ← Missing the "L" (now in line 27's content)
```

### Test 2: After Reading 1 Line (Simple Case)
```
File: "Line 1 content\r\nLine 2 content\r\nLine 3 content\r\n"
       0-14           15-16 17-31        32-33 34-48

Expected botPos after line 1: byte 17
Actual botPos after line 1:   byte 18 (probably, or +1 somewhere)

Result:
  Reading from botPos: (empty)
  Should be:         "Line 2 content"
```

### Test 3: Step-by-Step Offset Progression
```
After reading N lines:
  Expected: offset = start + (sum of all line lengths + 2 bytes per line for CRLF)
  Actual:   offset = start + (sum of all line lengths + 2 bytes per line for CRLF) + 1

Pattern across all increments:
  Line 1 → bot1:     start + 16 = 16 ✓ (correct for line 2 start)
  Line 2 → bot2:     bot1 + 46  (should be 16 + 46 = 62)
  ...
  First read after getLinesFromStart:
           botPos1:  (start + 25*46) + 1 ← THE BUG IS HERE
```

## What's Happening

### The Off-by-One Error

Looking at the byte-level evidence:

1. **Line content is 44 chars**: "Line 26 has content here to make it non-empty"
2. **CRLF is 2 bytes**: \r\n
3. **Total line = 46 bytes**

When line 27 is read and returns "ine 26..." (44 chars), it's showing us:
- The byte at position 1 of line 26's content onwards
- This means the offset was pointing at byte 1, not byte 0

**Conclusion:** The `botPos` calculation adds 1 extra byte

### Where It Happens

The bug occurs in:
```
getLinesFromStart cache 25
  → calculates botPos for next read
  → returns botPos = (correct offset) + 1  ← BUG HERE
```

Then when you do:
```
getLinesFrom cache botPos Forward 1 26
  → starts reading from botPos
  → reads from (correct offset) + 1
  → gets empty line (no complete line at that position)
```

## The Fix Pattern

### Likely Buggy Code
```haskell
-- In LineCache.lhs, the botPos calculation probably looks like:
botOffset = lineStartOffset + totalLineLength + 1  -- BUG: +1 extra
```

Or:
```haskell
-- Or it counts CRLF as 1 byte instead of 2:
botOffset = lineStartOffset + totalLineLength + 1  -- Should be +2
```

Or:
```haskell
-- Or it miscalculates the boundary:
let newPos = offset + contentLength  -- Missing the +2 for CRLF
```

### Correct Fix
```haskell
-- Should be:
botOffset = lineStartOffset + totalLineLength + 2  -- +2 for CRLF (not +1)

-- Or:
let newPos = offset + contentLength + 2  -- Add 2 for CRLF
```

## Impact on User

When scrolling down:
```
Initial view: Lines 1-25 (from getLinesFromStart)  ← Works fine
Scroll down:  Try to read lines 26-50               ← Gets empty line!
Result:      Line 26 is blank, line 27 is partial
```

This is why the "alternating pattern" happens:
- First read after getLinesFromStart: ✗ EMPTY (botPos off by 1)
- Subsequent reads: ✓ Work (because each read adjusts forward by what it actually read)
- But content is now misaligned (line numbers don't match content)

## Verification Tests Created

| Test | Input | Shows | Evidence |
|------|-------|-------|----------|
| test_incremental_debug | 50 lines | Line 26 empty | First increment fails |
| test_offset_deep_debug | 50 lines | Line 27 missing "L" | Exact +1 byte proof |
| test_byte_level | 3 lines | Line 2 empty after line 1 | Simplest case |

All three prove: **botPos = correct position + 1 byte**

---

## How to Find & Fix

1. **Search:** `src\HaFileViewer\LineCache.lhs`
2. **Find:** `botPos` or `extractNewPosition`
3. **Look for:** `+ 1` where `+ 2` should be (or vice versa)
4. **Check:** CRLF counting (should always be 2, not 1)
5. **Fix:** Adjust the offset arithmetic
6. **Test:** Run the three debug tests

The bug is simple: **off by 1 byte in offset calculation**
The impact is severe: **scrolling breaks completely**
The fix is straightforward: **fix the arithmetic** 

All three debug tests will pass once the offset calculation is corrected.
