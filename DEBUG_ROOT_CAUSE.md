# CRITICAL DISCOVERY: Empty Lines Root Cause

## The Bug

When scrolling down, the viewport returns empty lines at **alternating odd positions**:
- **Line 26**: len=23 (SHORT LINE - possible JSON fragment)
- **Line 27**: len=0 ← **EMPTY** (should have content)
- **Line 28**: len=243 (correct content)
- **Line 29**: len=0 ← **EMPTY** (should have content)
- **Line 30**: len=223 (correct content)
- **Line 31**: len=0 ← **EMPTY** (should have content)
- **Line 32**: len=242 (correct content)
- **Line 33**: len=0 ← **EMPTY** (should have content)

## Pattern Analysis

1. **Direct getLinesFromStart gives correct data:**
   - Line 26 len=243 (correct)
   - Line 27 len=223 (correct)
   - Line 28 len=242 (correct)
   - Line 29 len=224 (correct)

2. **But scrollDown gives wrong data:**
   - Line 26 len=23 (SHORT!)
   - Line 27 len=0 (EMPTY!)
   - Line 28 len=243 (sometimes correct)
   - Line 29 len=0 (EMPTY!)

## Root Cause: shiftViewportDown

The problem is in the **shiftViewportDown** operation in ViewState.hs:

```haskell
shiftViewportDown :: [LineWithNumber] -> LineWithNumber -> Int -> [LineWithNumber]
shiftViewportDown viewport newLine maxSize = 
  take maxSize (drop 1 viewport ++ [newLine])
```

The issue:
1. When scrolling down, we drop the first line from the current viewport
2. We append the new line returned from `getLinesFrom`
3. But `getLinesFrom` is returning **WRONG data** for certain line numbers
4. The pattern shows lines at odd positions (27, 29, 31, 33...) are getting corrupted

## getLinesFrom Bug

Looking at the scroll operation in Operations.hs:

```haskell
(moreLines, topPos, bottomPos) <- getLinesFrom cache 
                                   (cursorBottomPosition cursor) 
                                   Forward 
                                   1 
                                   startLineNum
```

The problem is likely in:
1. **Position tracking** - cursorBottomPosition may not be accurate
2. **Direction handling** - Forward direction with certain positions
3. **Line numbering calculation** - startLineNum calculation might be off

## Evidence

From debug output:

```
Initial viewport (lines 1-30): ALL CORRECT
  Line 26: len=243 ✓
  Line 27: len=223 ✓
  Line 28: len=242 ✓
  Line 29: len=224 ✓

After scroll 1 (line 26 dropped): 
  New line 26: len=23 ✗ WRONG (truncated?)
  
After scroll 2:
  New line 27: len=0 ✗ EMPTY (should be 223)
  
After scroll 3:
  New line 28: len=243 ✓ CORRECT (from initial cache?)
```

## Hypothesis

The issue appears to be a **cache mismatch or position offset error**:
1. Initial cache read gives correct lines 1-30
2. When asking for "next line after 30", something goes wrong
3. Every OTHER line from that point forward returns empty or truncated
4. The viewport shifts, dropping correct data and adding corrupted data

This suggests:
- **Byte offset calculation is off by half a line**
- **Line boundary detection in bidirectional scanner is skipping lines**
- **Cache positions not being updated correctly during scroll**

## Next Investigation

Need to check:
1. The `getLinesFrom` function implementation
2. How `cursorBottomPosition` is tracked and updated
3. The bidirectional scanner's line boundary detection
4. Offset calculations in the LineCache when reading forward
