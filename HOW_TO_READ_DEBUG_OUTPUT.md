# How to Read the Debug Output

## Quick Start

1. **Open** `debug_viewport_output.txt`
2. **Find** the section "=== DEBUG: VIEWPORT AT LINE 30 ==="
3. **Look** at "Line Details" section
4. **Notice**:
   - Line 26: len=23 (should be 243 - TRUNCATED)
   - Line 27: len=0 (should be 223 - EMPTY)
   - Line 28: len=243 (correct)
   - Line 29: len=0 (should be 224 - EMPTY)
   - Line 30: len=223 (should be 243 - SHIFTED)

This is the smoking gun! 🔫

---

## Understanding the Debug Output

### Section 1: Initial Scroll
```
Initial viewport: 1 to 25     ✓ Correct
Scrolling down until line 100 is visible...
  After scroll: lines 2 to 26
  >>> FOUND EMPTY LINES IN VIEWPORT <<<   ← First empty line detected
```

This shows empty lines appear almost immediately during scrolling.

### Section 2: Viewport at Line 30
```
=== DEBUG: VIEWPORT AT LINE 30 ===

Cursor State:
  firstLine: 6
  lastLine: 30
  origin: FromStart

Viewport Content:
Total lines in viewport: 25
Empty lines: 2
```

Key data:
- Viewport has 25 lines (correct size)
- Contains 2 empty lines (BAD)

### Section 3: Line Details
```
Line  6: len=242 02/22/2026 00:47:13.169...  ✓ Normal
Line  7: len=223 02/22/2026 00:47:13.169...  ✓ Normal
...
Line 26: len=23 xy.","Proxy":"Server"}       ✗ TRUNCATED (JSON fragment?)
Line 27: len=0  <EMPTY>                      ✗ EMPTY LINE
Line 28: len=243 02/22/2026 00:47:13.169...  ✓ Normal
Line 29: len=0  <EMPTY>                      ✗ EMPTY LINE
Line 30: len=223 02/22/2026 00:47:13.169...  ~ Shifted (should be 243)
```

Pattern explained:
1. First new line (26) is truncated to 23 characters
   - This is from JSON: `xy.","Proxy":"Server"}`
   - Suggests byte position jumped into middle of line
   
2. Next line (27) is completely empty (0 bytes)
   - Should have 223 characters
   - Getting nothing instead
   
3. Pattern alternates: truncated/empty, normal, empty, shifted...

---

## Understanding debug_deep_dive_output.txt

```
1. Total lines in file:
   0           ← Note: getTotalLines returns 0 (might be unimplemented)

2. Getting lines 1-30 from start:
   Received 30 lines
   1. Line 1 len=69 Timestamp Process TID...   ✓ Correct
   ...
   26. Line 26 len=243...                      ✓ Correct
   27. Line 27 len=223...                      ✓ Correct
   28. Line 28 len=242...                      ✓ Correct
   29. Line 29 len=224...                      ✓ Correct
   
   ← Direct cache read gives CORRECT data!

3. Scrolling down and checking each new line:
   Scroll 1: Got line 26 len=23               ✗ WRONG (should be 243)
   Scroll 2: Got line 27 len=0 *** EMPTY ***  ✗ WRONG (should be 223)
   Scroll 3: Got line 28 len=243              ✓ Correct
   Scroll 4: Got line 29 len=0 *** EMPTY ***  ✗ WRONG (should be 224)
   Scroll 5: Got line 30 len=223              ✗ WRONG (should be 243)
   
   ← Scroll returns WRONG data!
```

This proves:
- **Direct cache read is fine** ✓
- **Scroll operation corrupts data** ✗
- **Bug is in the scroll logic** (Operations.hs or similar)

---

## What This Tells Us

### The Evidence Chain

1. **File content is correct**
   - 71,356 lines, 0 empty
   - Verified with check_empty_lines.hs

2. **Direct cache reads are correct**
   - getLinesFromStart returns all 30 lines correctly
   - No empty lines when reading directly

3. **Scroll operations are corrupted**
   - scrollDown returns wrong data
   - Every other line is empty
   - First line is truncated

4. **Systematic pattern**
   - Not random
   - Alternating empty/wrong/correct pattern
   - Suggests **off-by-half-line offset error**

---

## The Root Cause (Hypothesis)

When scrollDown is called:

```
Initial state:
  viewport has lines 1-25
  cursorBottomPosition points to end of line 25
  
Call scrollDown:
  ask for line 26 starting from cursorBottomPosition
  But cursorBottomPosition is WRONG - off by ~half a line
  
Result:
  instead of reading from byte offset of line 26
  we read from middle of line 26
  get 23 chars from end of line 26
  then line 27 returns empty
  position goes out of sync
```

---

## How to Fix (For Developers)

1. **Check Operations.scrollDown**
   - How is `startLineNum` calculated?
   - Is `cursorBottomPosition` accurate?
   - Debug: print actual byte position being read

2. **Verify LineCache.getLinesFrom**
   - Trace byte positions for lines 26-30
   - Check offset calculation
   - Verify line boundary detection

3. **Test with logging**
   ```haskell
   putStrLn $ "Reading from position: " ++ show pos
   putStrLn $ "Expected line: " ++ show startLineNum
   putStrLn $ "Got line: " ++ show (fst (head moreLines))
   putStrLn $ "Got length: " ++ show (T.length (snd (head moreLines)))
   ```

4. **Fix cursor position tracking**
   - Ensure topPos and bottomPos are updated correctly
   - Verify they point to correct byte offsets

---

## Summary for Code Review

**What we know:**
- ✓ Empty lines appear at lines 27, 29, 31...
- ✓ Pattern is consistent and reproducible
- ✓ File has no empty lines
- ✓ Direct cache reads work
- ✗ Scroll reads fail
- ✗ Byte offset is off by ~half a line
- ✗ scrollUp stops at line 15 instead of 1

**What to investigate:**
- cursorBottomPosition calculation/tracking
- Byte offset in forward scanning
- Line boundary detection
- Position update after each getLinesFrom call

**Expected when fixed:**
- All lines returned have correct content
- No empty lines in viewport
- Scroll-up reaches line 1
- Scroll-down shows correct content throughout file

