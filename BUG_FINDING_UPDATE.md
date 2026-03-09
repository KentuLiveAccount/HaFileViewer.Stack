# Bug Finding Update: User Observation Reveals True Bug

**Date:** 2026-03-09  
**Critical Observation:** User noticed empty lines going DOWN, but NO empty lines going UP

---

## Test Results Confirm User Observation

**Test:** `test_forward_backward.hs`

```
1. Initial read (1-30): ✓ All correct, no empty lines

2. Forward read (line 31): ✓ Got text (len=28)

3. Forward read again (line 32): ✗ EMPTY LINE <<<

4. Backward read (line 29): ✗ Got 0 lines

5. Backward read again (line 28): ✗ Got 0 lines
```

---

## Revised Analysis

### Previous Hypothesis: WRONG
- Claimed: Backward's `reverse` operation causes mismatch
- Reality: Backward returns 0 lines (fails silently)

### Actual Bug Pattern:

#### Forward Direction (scrollDown):
- Read 1: Works ✓
- Read 2: Empty ✗
- Read 3: Works (presumably)
- Read 4: Empty ✗
- **Pattern:** Alternating success/empty

#### Backward Direction (scrollUp):
- Returns 0 lines completely
- Causes scroll operation to be no-op
- **Result:** No visual bug (no empty lines shown)
- **BUT:** Explains why scroll up stops at line 15 (can't scroll further)

---

## Why User Saw What They Saw

**Going DOWN:**
- `scrollDown` calls `getLinesFrom(..., Forward, 1, ...)`
- Forward alternates: text, empty, text, empty...
- Empty lines appear in viewport every other scroll

**Going UP:**
- `scrollUp` calls `getLinesFrom(..., Backward, 1, ...)`
- Backward returns 0 lines
- When 0 lines returned, `scrollUp` does nothing (line 123-124 in Operations.hs)
- No empty lines appear (no change to viewport)
- Eventually can't scroll further → stops at line 15

---

## Root Cause Location

**NOT** the line number calculation!

**Likely:** Position/offset tracking between successive `getLinesFrom` calls

**Evidence:**
1. Initial `getLinesFromStart` works perfectly (30 lines, all correct)
2. First `getLinesFrom` from bottomPosition works (line 31 returned)
3. Second `getLinesFrom` from new bottomPosition fails (empty)
4. `getLinesFrom` with Backward fails immediately (0 lines)

**Hypothesis:** The `topPosition` and `bottomPosition` returned by `getLinesFrom` are incorrect, causing subsequent reads to fail.

---

## Next Investigation Steps

1. **Print the positions** being returned and used:
   - What offset is in bottomPosition after read 1?
   - What offset is in bottomPosition after read 2?
   - Are they advancing correctly?

2. **Check LineCache.lhs getLinesFrom**:
   - Lines 374-433: How are topPosition/bottomPosition calculated?
   - Are offsets being updated correctly after Forward reads?
   - Why does Backward return 0 lines?

3. **Verify offset calculations**:
   - Is `startOffset` correct?
   - Is `topOffset`/`bottomOffset` calculation buggy?

---

**Status:** Initial hypothesis WRONG. Need to investigate position tracking.
