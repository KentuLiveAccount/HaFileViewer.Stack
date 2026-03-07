# Manual Test Cases for Bug #5 and Bug #6

## Bug #5: Scrolling from end doesn't work

**Test with actual application:**
```
stack run cui-log-viewer test-sample.txt
```

### Test 5.1: Up arrow after 'G'
1. Press 'G' to jump to end
2. **Observe**: Bottom line should be -1
3. Press Up arrow ONCE
4. **Expected**: Top line should be -26 (or similar, one line earlier)
5. **Bug if**: Nothing changes OR wrong line numbers appear

### Test 5.2: Down arrow after 'G'
1. Press 'G' to jump to end
2. Press Down arrow ONCE
3. **Expected**: Nothing changes (already at end)
4. **Bug if**: New line appears (e.g., -24 after -1)

---

## Bug #6: Wrong line after single scroll

**Test with actual application:**
```
stack run cui-log-viewer test-sample.txt
```

### Test 6.1: Down then Up
1. Start at beginning (should show lines 1-25)
2. Press Down arrow ONCE
3. **Observe**: Should show lines 2-26
4. Press Up arrow ONCE  
5. **Expected**: Should show lines 1-25 (back to start)
6. **Bug if**: First line shows line 26 (or any number other than 1)

### Test 6.2: Multiple Down then Up
1. Start at beginning
2. Press Down arrow 5 times (should show lines 6-30)
3. Press Up arrow 5 times
4. **Expected**: Back to lines 1-25
5. **Bug if**: First line is not 1

---

## Why LineCache tests pass but UI fails

**The disconnect:**
- LineCache API tests (`test_bug_fixes.hs`) test the **raw API**: `getLinesFrom(pos, direction, count)`
- These tests pass because LineCache now correctly generates consecutive line numbers
- But **CUILogViewer** (Main.hs) has its own bugs:
  - Bug #6: `lpLineNum` tracks "total read" not "first visible line"
  - This causes position tracking to drift during scrolling

**What needs fixing:**
- LineCache: ✅ Fixed (consecutive negative numbers)
- CUILogViewer: ❌ Still broken (lpLineNum semantics wrong)

---

## Current Status

**What was fixed:**
- ✅ LineCache generates correct consecutive negative numbers for FromEnd
- ✅ Line number calculation for FromEnd origin fixed

**What's still broken:**
- ❌ lpLineNum in LinePosition tracks "total lines read" not "first visible"
- ❌ This causes CUILogViewer scrolling to show wrong line numbers
- ❌ Affects both Bug #5 and Bug #6

**Next steps:**
1. Fix Bug #6 first (change lpLineNum semantics to "first visible line")
2. This should fix both Bug #5 and Bug #6 in the UI
3. Verify with manual testing using these test cases
