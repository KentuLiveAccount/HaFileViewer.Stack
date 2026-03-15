# Scanner Offset Calculation Analysis

## Test Setup

File content: `"A\r\nB\r\nC\r\n"` (9 bytes total)

Byte-by-byte breakdown:
```
[0] = 65 ('A')
[1] = 13 (CR)
[2] = 10 (LF)
[3] = 66 ('B')
[4] = 13 (CR)
[5] = 10 (LF)
[6] = 67 ('C')
[7] = 13 (CR)
[8] = 10 (LF)
```

## Key Finding: The Scanner Offsets ARE Correct!

### What BS.split does
When we split `"A\r\nB\r\nC\r\n"` on LF byte (10), we get:
```
["A\r", "B\r", "C\r", ""]
```

**Important**: BS.split **removes the delimiter (LF) but keeps everything else (including CR)**

### Offset Calculation Formula

From `calculatePieceOffsets`:
```haskell
calculatePieceOffsets startOffset pieces =
  let go currentOffset (piece:rest) =
        let nextOffset = currentOffset + fromIntegral (BS.length piece) + 1  -- +1 for LF
        in currentOffset : go nextOffset rest
  in go startOffset pieces
```

For our pieces with lengths [2, 2, 2, 0]:
```
Offset[0] = 0
Offset[1] = 0 + 2 + 1 = 3  (piece "A\r" is 2 bytes, +1 for LF)
Offset[2] = 3 + 2 + 1 = 6  (piece "B\r" is 2 bytes, +1 for LF)
Offset[3] = 6 + 2 + 1 = 9  (piece "C\r" is 2 bytes, +1 for LF)
```

### After stripCR

`stripCR` removes trailing CR from each piece:
```
Before: ["A\r", "B\r", "C\r", ""]
After:  ["A",  "B",  "C",  ""]
```

**CRITICAL**: The offsets are NOT recalculated after stripCR.
The offsets remain: [0, 3, 6, 9]

### Verification

Expected vs Actual:
- Line "A" at offset 0: ✓ CORRECT
- Line "B" at offset 3: ✓ CORRECT  
- Line "C" at offset 6: ✓ CORRECT
- Empty at offset 9: ✓ CORRECT

## Conclusion

**The scanner IS calculating offsets correctly!**

The offsets properly identify where each line starts in the file, accounting for the CRLF delimiters.

## The Bug Must Be Elsewhere

Since the scanner offsets are correct, the bug must be in:

1. **LineCache's `bottomOffset` formula** - How it uses these offsets
2. **Viewport calculation** - How it interprets the bottom offset
3. **Edge cases in partial line handling** - Special cases for line boundaries

### Next Steps

Check:
1. How does LineCache use the offsets from the scanner?
2. What is the `bottomOffset` formula?
3. Are there edge cases where offset handling breaks?
4. Is the issue in how backward scanning calculates offsets for partial lines?
