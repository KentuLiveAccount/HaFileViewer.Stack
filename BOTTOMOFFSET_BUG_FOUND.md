# BOTTOMOFFSET BUG FOUND!

## Summary

The bug is in the `bottomOffset` calculation in `LineCache.lhs` at lines 312-316, 361-365, and 425-430.

## The Bug

The formula:
```haskell
bottomOffset = lastOff + fromIntegral (BS.length $ TE.encodeUtf8 lastText) + 1
```

This assumes **Unix LF-only** line endings. The `+1` accounts for the `\n` byte.

## Problem with CRLF Files

For CRLF files (Windows line endings), the formula is **OFF BY ONE**:

### Example:
File content: `"A\r\nB\r\nC\r\n"` (9 bytes total)

Scanner returns (correctly):
```
[("A", 0), ("B", 3), ("C", 6)]
```

Current formula for last line "C":
```
bottomOffset = 6 + 1 + 1 = 8
```

**But the file has 9 bytes!** The last byte is at position 8 (0-indexed), so position 9 is the EOF.

### Breakdown:
- Position 6: 'C' (1 byte)
- Position 7: '\r' (1 byte)  
- Position 8: '\n' (1 byte)
- Position 9: EOF

Current formula gives 8, but should give 9.

## Root Cause

The scanner correctly identifies that:
- Line "C" starts at byte 6
- The scanner stores offsets BEFORE stripping CR

But LineCache's `bottomOffset` formula only adds 1 (for \n), not 2 (for \r\n).

## Where It's Used

The `bottomOffset` is returned as a `LinePosition` to the UI layer, which uses it to:
1. Track the bottom of the visible viewport
2. Calculate how far down to scroll
3. Determine how many lines to read next

When `bottomOffset` is wrong, the viewport calculation gets confused about line positions.

## How to Fix

Need to detect if the file uses CRLF and add 2 instead of 1:

```haskell
-- Option 1: Store line ending info during scanning
bottomOffset = lastOff + fromIntegral (BS.length $ TE.encodeUtf8 lastText) + 
               (if useCRLF then 2 else 1)

-- Option 2: Look at the next byte in the file after the line
-- Option 3: Make the scanner return line ending information
```

## Affected Code Locations

1. `LineCache.lhs:312-316` - `getLinesFromStart`
2. `LineCache.lhs:361-365` - `getLinesFromEnd`
3. `LineCache.lhs:425-430` - `getLinesFrom`

All three locations use the same buggy formula.

## Test Case

```
File: "A\r\nB\r\nC\r\n" (9 bytes)
Scanner returns: [("A", 0), ("B", 3), ("C", 6)]
Current formula bottomOffset = 6 + 1 + 1 = 8
Correct bottomOffset should be = 9
```

This is a **critical bug** that affects all CRLF line ending support!
