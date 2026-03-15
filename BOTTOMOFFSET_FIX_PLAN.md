# Fix Plan for bottomOffset Bug

## The Problem

The current formula only accounts for `\n` (1 byte):
```haskell
bottomOffset = lastOff + fromIntegral (BS.length $ TE.encodeUtf8 lastText) + 1
```

For CRLF files, this is off by one because it doesn't account for `\r` (another byte before `\n`).

## Solution Approach

We need to detect if the file uses CRLF and add 2 instead of 1.

### Option A: Peek at File (Simplest)
```haskell
calculateBottomOffset :: Handle -> (T.Text, Offset) -> IO Offset
calculateBottomOffset h (lastText, lastOff) = do
  let textLen = fromIntegral (BS.length $ TE.encodeUtf8 lastText)
      potentialCRPos = lastOff + textLen
  -- Peek at the byte after the text
  hSeek h AbsoluteSeek (fromInteger potentialCRPos)
  nextByte <- BS.hGet h 1
  let hasCR = not (BS.null nextByte) && BS.head nextByte == 13  -- 13 is '\r'
  return $ lastOff + textLen + (if hasCR then 2 else 1)
```

### Option B: Modify Scanner to Return Line Ending Info
```haskell
type LineWithEnding = (T.Text, Offset, Bool)  -- (text, offset, hasCRLF)
scanLinesWithLineEndings :: Direction -> Integer -> ReadFn -> Int -> IO [(T.Text, Offset, Bool)]
```

This is cleaner but requires changing the API.

### Option C: Global File Characteristic Detection
Store whether the file uses CRLF or LF in the LineCache state during first scan.

## Implementation Plan

I'll use **Option A** (peek at file) because:
1. No API changes needed
2. Minimal code changes
3. Works correctly for any mix of line endings in a single file
4. Has negligible performance impact

## Files to Modify

1. `LineCache.lhs`: Create helper function and apply it to:
   - Line 312-316: `getLinesFromStart`
   - Line 361-365: `getLinesFromEnd`
   - Line 425-430: `getLinesFrom`

## Helper Function

```haskell
-- Calculate the offset of the byte AFTER a line's newline sequence
-- Accounts for both Unix (LF) and Windows (CRLF) line endings
calculateBottomOffsetFromLine :: Handle -> (T.Text, Offset) -> IO Offset
calculateBottomOffsetFromLine h (lastText, lastOff) = do
  let textLen = fromIntegral (BS.length $ TE.encodeUtf8 lastText)
      nextBytePos = lastOff + textLen
  -- Check if the next byte is CR (Windows line ending)
  hSeek h AbsoluteSeek (fromInteger nextBytePos)
  nextByte <- BS.hGet h 1
  let hasCR = not (BS.null nextByte) && BS.head nextByte == 13  -- ASCII 13 is '\r'
  return $ nextBytePos + (if hasCR then 2 else 1)  -- +2 for \r\n or +1 for \n
```

## Test Case

```
File: "A\r\nB\r\nC\r\n" (9 bytes)
Scanner returns: ("C", 6)
Text length: 1
CR at position 7: Yes
Result: 6 + 1 + 2 = 9 ✓

File: "A\nB\nC\n" (6 bytes)
Scanner returns: ("C", 4)
Text length: 1
CR at position 5: No
Result: 4 + 1 + 1 = 6 ✓
```

## Status

Ready to implement once approved.
