# Bug Fix Summary: Bug #4 and Bug #5

## Date
Fixed on current session

## Bugs Fixed

### Bug #4: Scrolling up after scrolling down causes line numbers to flip negative
**Status**: ✅ FIXED

**Problem**: When starting from the beginning (g), scrolling down, then scrolling up, line numbers would flip from positive to negative.

**Root Cause**: The `lpDirection` field was being used for two different concepts:
1. Origin (where did user start? FromStart vs FromEnd)
2. Scroll direction (which way is user moving? Forward vs Backward)

When scrolling changed direction, `lpDirection` would change from Forward to Backward, causing line numbers to flip sign.

### Bug #5: After 'G', up arrow doesn't scroll (inverted behavior)
**Status**: ✅ FIXED

**Problem**: After jumping to end with 'G', pressing up arrow wouldn't show previous lines.

**Root Cause**: Same as Bug #4 - the direction field was being confused between origin and scroll direction.

## Solution

### 1. Renamed Field for Clarity
Changed `lpDirection` to `lpOrigin` to make it clear this represents the ORIGIN, not the scroll direction.

```haskell
-- Before:
data LinePosition = LinePosition 
  { lpOffset    :: Offset
  , lpLineNum   :: Integer
  , lpDirection :: Direction  -- WRONG: Mixed two concepts
  }

-- After:
data LinePosition = LinePosition 
  { lpOffset    :: Offset
  , lpLineNum   :: Integer
  , lpOrigin    :: ScanOrigin  -- CORRECT: Just tracks origin
  }
```

### 2. Added New ScanOrigin Type
```haskell
data ScanOrigin = FromStart | FromEnd
  deriving (Show, Eq)
```

This makes it explicit that we're tracking WHERE the user started, not HOW they're scrolling.

### 3. Updated getLinesFrom to Keep Origin Stable
The critical fix: `lpOrigin` NEVER changes during scrolling. It's set once (at start) and preserved.

```haskell
getLinesFrom lc (LinePosition startOffset currentLineNum origin) dir count = do
  -- ... scanning logic ...
  
  -- Calculate line numbers based on ORIGIN (determines sign)
  let startLineNum = case (origin, dir) of
        (FromStart, Forward)  -> currentLineNum
        (FromStart, Backward) -> currentLineNum - fromIntegral count
        (FromEnd, Forward)    -> currentLineNum + fromIntegral count
        (FromEnd, Backward)   -> currentLineNum - 1
  
  let lineNumbers = case origin of
        FromStart -> calculateForwardLineNumbers startLineNum count  -- Always positive
        FromEnd   -> calculateBackwardLineNumbers ...  -- Always negative
  
  -- Keep SAME origin (don't change it!)
  let newPosition = LinePosition newOffset newLineNum origin
```

## Changes Made

### Files Modified
1. `src/HaFileViewer/LineCache.lhs`
   - Added `ScanOrigin` type (FromStart | FromEnd)
   - Renamed `lpDirection` → `lpOrigin` in LinePosition
   - Updated `getLinesFromStart` to use `FromStart`
   - Updated `getLinesFromEnd` to use `FromEnd`
   - Fixed `getLinesFrom` to:
     - Accept scroll direction as separate parameter
     - Use `lpOrigin` for line number sign
     - Keep origin stable (never change it)

## Testing

### Automated Tests
All existing tests pass:
- test 1: 23 examples, 0 failures
- test 2: 10 examples, 0 failures  
- test 3: 22 examples, 0 failures
- test 4: 21 examples, 0 failures

### Manual Bug Tests
Created `test_bug_fixes.hs` to verify both bugs:

**Bug #4 Test Result**: PASS
- Started from beginning (lines 1, 2, 3...)
- Scrolled down multiple times
- Scrolled UP (Backward)
- Result: Line numbers stayed POSITIVE ✅

**Bug #5 Test Result**: PASS
- Started from end with getLinesFromEnd (lines -3, -2, -1)
- Scrolled UP (Backward)
- Result: Shows previous lines (-6, -5, -4...) ✅

## Impact

### Behavior Changes
- **Line numbering now stable**: Line numbers maintain their sign based on origin, regardless of scroll direction
- **Scrolling works in both directions**: Can freely scroll up/down without breaking line numbers
- **'g' and 'G' work correctly**: Starting from beginning or end preserves the correct numbering scheme

### No Breaking Changes
- API remains the same (just internal field rename)
- All existing tests pass
- Backward compatible with existing code

## Verification Commands

```powershell
# Build
stack build --fast

# Run all tests
stack test

# Run manual bug test
stack exec -- ghc -o test_bug_fixes test_bug_fixes.hs
.\test_bug_fixes.exe

# Test in CUI viewer
stack run cui-log-viewer test-scroll-bug.txt
# Then try: scroll down, scroll up, press 'G', press up arrow
```

## Key Insight

The fundamental issue was **conceptual confusion**: We were using a single field (`lpDirection`) to represent two different concepts:
1. **Origin** (invariant): Where did we start? This should NEVER change.
2. **Scroll Direction** (variant): Which way are we currently moving?

By separating these concepts, we fixed both bugs and made the code clearer.

---

**Status**: Both bugs FIXED and verified ✅
