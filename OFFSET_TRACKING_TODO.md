# Problematic Code Locations

## 1. LineCache.lhs - findStartOffset (lines 276-282)
**Current behavior:** Always returns (0, 0) when sparse index is empty
**Problem:** Sparse index is never populated with real offsets
**Why:** prepareFinalLinesWithOffsets calculates wrong offsets

## 2. BidirectionalScanner.lhs - prepareFinalLinesWithOffsets (lines 309-330)
**Current behavior:** Encodes Text back to ByteString to calculate offsets
**Problem:** 
  - Calculates AFTER scanning (too late)
  - Text -> ByteString encoding may differ from original
  - Doesn't account for complex line processing (partials, EOF handling)

## Root Cause:
The scanning process is:
  1. Read ByteString chunks at known offsets
  2. Split into lines (ByteString)
  3. Process (combine partials, reorder for backward)
  4. Convert to Text
  5. Try to calculate offsets (TOO LATE - original byte positions lost)

## What Should Happen:
  1. Read chunks at known offsets
  2. Split into lines, TRACK OFFSET OF EACH LINE
  3. Process while preserving offset information
  4. Convert to Text with correct offsets

## Where to Fix:
- processChunk (line ~288) - needs to track line offsets as it extracts lines
- ScanState (line 210) - needs to store [(ByteString, Offset)] instead of [ByteString]
- All strategy functions that work with lines
