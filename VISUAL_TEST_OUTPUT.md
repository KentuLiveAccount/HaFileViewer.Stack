# Visual Test Output - Scanner Offset Investigation

## Test Output 1: test_scanner_simple.hs

```
=== Scanner Offset Calculation Test ===

Input pieces (after split on LF):
  [0]: "A" (length 1)
  [1]: "B" (length 1)
  [2]: "C" (length 1)
  [3]: "" (length 0)

Calculated offsets (formula: offset[i] = sum of (len(p[0..i-1]) + 1) for each p):
  [0]: 0
  [1]: 2
  [2]: 4
  [3]: 6

ACTUAL pieces (with CR, since BS.split keeps everything except LF):
  [0]: "A\r" (length 2)
  [1]: "B\r" (length 2)
  [2]: "C\r" (length 2)
  [3]: "" (length 0)

ACTUAL calculated offsets:
  [0]: 0
  [1]: 3
  [2]: 6
  [3]: 9

After stripCR:
  [0]: "A" (length 1)
  [1]: "B" (length 1)
  [2]: "C" (length 1)
  [3]: "" (length 0)

KEY FINDING:
====================

When BS.split is used on CRLF-terminated lines:
  - Input: "A\r\nB\r\nC\r\n"
  - Pieces with CR: ["A\r", "B\r", "C\r", ""]
  - Calculated offsets: [0, 3, 6, 9]

After stripCR:
  - Pieces: ["A", "B", "C", ""]
  - Offsets UNCHANGED: [0, 3, 6, 9]

This means:
  - stripCR is applied to pieces AFTER offset calculation ✓
  - Offset 3 points to 'B' in the file ✓
  - BUT: offset includes the \r if present in original!

The scanner IS calculating offsets correctly!
The issue might be in LineCache's bottomOffset formula.
```

## Test Output 2: test_bottomoffset_bug.hs

```
=== Testing bottomOffset Formula Bug ===

Lines with offsets returned by scanner:
  ("A", 0)
  ("B", 3)
  ("C", 6)

LineCache formula calculation:
  lastText = "C"
  lastOff = 6
  textBytes = BS.length (TE.encodeUtf8 lastText)
            = BS.length "C"
            = 1
  bottomOffset = 6 + 1 + 1
              = 8

But wait! The lastOff is the BYTE OFFSET of the last line START!
  - lastOff = 6 means 'C' starts at byte 6
  - 'C' is 1 byte long
  - So 'C' occupies byte 6
  - The byte AFTER 'C' is at position 7
  - Then comes \r (byte 7) and \n (byte 8)
  - So bottomOffset should be 9 (first byte after the line's \r\n)

But the formula gives: 8
Expected value: 9 (file size)

✗ FAIL: Result is 8, should be 9
  OFF BY ONE FOR CRLF FILES!
```

## Test Output 3: test_comprehensive_scanner.hs

```
=== Comprehensive Scanner & bottomOffset Test ===

TEST 1: Scanner Offset Calculations
===================================
File content: "A\r\nB\r\nC\r\n" (9 bytes)
After BS.split on LF: ["A\r","B\r","C\r",""]

Calculated offsets (what scanner returns):
  Offset[0] = 0
  Offset[1] = 3
  Offset[2] = 6
  Offset[3] = 9

✓ PASS: Scanner offsets are CORRECT
```

## Analysis Summary

### What the Tests Show:

1. **Scanner offsets are 100% CORRECT**
   - Accounts for both the line content AND the line ending bytes
   - Pieces include CR before stripping: ["A\r", "B\r", "C\r", ""]
   - Offsets correctly add 1 for each delimiter (LF)
   - Final offsets: [0, 3, 6, 9] ✓

2. **LineCache formula is WRONG**
   - Only adds 1 to lastOff
   - Doesn't account for the CR byte
   - Results in 8 instead of 9 ✗

3. **The Fix is Clear**
   - Need to detect if next byte is CR
   - Add 2 instead of 1 when CR is present
   - Add 1 when only LF is present

## Implementation Checklist

- [ ] Create helper: `calculateBottomOffsetFromLine :: Handle -> (T.Text, Offset) -> IO Offset`
- [ ] Check byte at position (lineOffset + lineLength)
- [ ] Return `+ 2` if byte is 13 (CR), else `+ 1`
- [ ] Replace formula at line 312-316
- [ ] Replace formula at line 361-365
- [ ] Replace formula at line 425-430
- [ ] Run tests
- [ ] Verify with CRLF file
- [ ] Verify with LF file

## Verification After Fix

### Expected for CRLF file "A\r\nB\r\nC\r\n":
```
Line "C" at offset 6
Text length: 1
Byte[7]: 0x0D (\r) - YES
Add: 2
Result: 6 + 1 + 2 = 9 ✓ (matches file size)
```

### Expected for LF file "A\nB\nC\n":
```
Line "C" at offset 4
Text length: 1
Byte[5]: 0x0A (\n) - NOT CR
Add: 1
Result: 4 + 1 + 1 = 6 ✓ (matches file size)
```

**Status: READY FOR IMPLEMENTATION** ✅
