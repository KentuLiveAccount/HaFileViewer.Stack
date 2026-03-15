# Scanner Offset Investigation - Testing Summary

## Test Results

### Test 1: Scanner Offset Calculations ✓ PASSED

Created `test_scanner_simple.hs`:
- Validates offset calculation algorithm
- Input: File content `"A\r\nB\r\nC\r\n"` (9 bytes)
- BS.split on LF produces: `["A\r", "B\r", "C\r", ""]`
- Calculated offsets: `[0, 3, 6, 9]`
- **Result: CORRECT** ✓

### Test 2: bottomOffset Formula Bug ✓ VERIFIED

Created `test_bottomoffset_bug.hs`:
- Demonstrates the off-by-one error
- Input: Last line `("C", 6)` with length 1
- Current formula: `6 + 1 + 1 = 8`
- File size: 9 bytes
- **Error magnitude: OFF BY 1** ✗

### Test 3: Comprehensive Test ✓ PASSED

Created `test_comprehensive_scanner.hs`:
- Validates offset calculations
- Offset[0] = 0 ✓
- Offset[1] = 3 ✓
- Offset[2] = 6 ✓
- Offset[3] = 9 ✓

## Key Findings

1. **Scanner offsets ARE correct**
   - Produces: [0, 3, 6, 9]
   - These represent actual byte positions in file
   - Accounts for both text and CR byte

2. **bottomOffset formula is WRONG**
   - Located in LineCache.lhs at 3 locations
   - Only adds 1 (for \n)
   - Needs to add 2 (for \r\n) in CRLF files

3. **Impact is significant**
   - Affects all CRLF file handling
   - Causes scroll position errors
   - Off-by-one in viewport calculations

## Detailed Bug Analysis

### Current Formula (WRONG for CRLF):
```
bottomOffset = lastOff + len(text) + 1
            = 6 + 1 + 1
            = 8
```

### File Structure:
```
Byte 0-2: A\r\n
Byte 3-5: B\r\n
Byte 6-8: C\r\n
Byte 9: EOF
```

### What Should Happen:
```
After reading line "C":
  lineOffset = 6
  lineLength = 1
  nextBytePos = 6 + 1 = 7
  Check byte[7] = 0x0D (\r) ? YES
  Then add 2 for \r\n
  bottomOffset = 6 + 1 + 2 = 9 ✓
```

### What Actually Happens:
```
bottomOffset = 6 + 1 + 1 = 8 ✗
File ends at byte 9, so we're one byte short!
```

## Test Coverage

| Aspect | Test | Result |
|--------|------|--------|
| Scanner offset calculation | test_scanner_simple.hs | ✓ PASS |
| Offset algorithm verification | Manual calculation | ✓ PASS |
| bottomOffset bug demonstration | test_bottomoffset_bug.hs | ✓ VERIFIED |
| Comprehensive test | test_comprehensive_scanner.hs | ✓ PASS |
| CRLF vs LF handling | Documented | ✓ READY |

## Conclusion

The investigation conclusively shows:

1. ✓ Scanner is working correctly
2. ✗ LineCache bottomOffset formula has a bug
3. ✓ Bug is clearly identified and reproducible
4. ✓ Fix is straightforward (detect CRLF)

The bug is in `LineCache.lhs` at:
- Line 312-316: `getLinesFromStart`
- Line 361-365: `getLinesFromEnd`
- Line 425-430: `getLinesFrom`

All three use the same buggy formula that doesn't account for CR in CRLF files.

## Next Steps

Implement the CRLF-aware bottomOffset calculation that:
1. Checks if next byte after line is CR (0x0D)
2. Adds 2 if CR is present
3. Adds 1 if only LF is present
4. Works correctly for both Unix and Windows line endings
