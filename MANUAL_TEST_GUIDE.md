# Phase 2 Implementation - Manual Test Guide

## Quick Verification

Run the following command:
```powershell
stack run cui-log-viewer test-phase2.txt
```

## Test Checklist

### ✅ Basic Startup
- [ ] Application starts without errors
- [ ] Lines 1-25 are displayed
- [ ] Line numbers appear correctly (1: Line 1:..., 2: Line 2:..., etc.)
- [ ] Status bar shows "Lines: 26+..."

### ✅ Jump to Start (press 'g')
- [ ] Displays lines 1-25
- [ ] Line 1 is at the top
- [ ] Status bar shows "Lines: 26+..."

### ✅ Jump to End (press 'G')
- [ ] Displays last 25 lines of file
- [ ] Line numbers are NEGATIVE (-25 to -1)
- [ ] Line -1 is the last line of the file
- [ ] Status bar shows "Lines: ...-25"

### ✅ Single Line Scroll Down (press ↓ or 'j')
From start position:
- [ ] Viewport shifts down by 1 line
- [ ] Top line changes from 1 to 2
- [ ] New line appears at bottom
- [ ] Line numbers increment correctly

### ✅ Single Line Scroll Up (press ↑ or 'k')
From line 26+:
- [ ] Viewport shifts up by 1 line
- [ ] New line appears at top
- [ ] Bottom line disappears
- [ ] Line numbers decrement correctly

### ✅ Page Down (press PgDn)
- [ ] Viewport advances by full page (25 lines)
- [ ] Line numbers jump by 25
- [ ] All 25 new lines appear

### ✅ Page Up (press PgUp)
- [ ] Viewport goes back by full page (25 lines)
- [ ] Line numbers decrease by 25
- [ ] All 25 previous lines appear

### ✅ Edge Cases
- [ ] Scrolling up from line 1 does nothing (stays at start)
- [ ] Scrolling down from last line does nothing (stays at end)
- [ ] PgUp from start does nothing
- [ ] PgDn from end does nothing

### ✅ Navigation Flow
Test this sequence:
1. Start application (lines 1-25)
2. Press 'G' (jump to end, lines -25 to -1)
3. Press 'g' (jump to start, lines 1-25)
4. Press PgDn several times (advance through file)
5. Press 'G' (should jump to end correctly)
6. Press PgUp several times (go back through file)
7. Press 'g' (should jump to start correctly)
- [ ] All transitions work smoothly
- [ ] Line numbers are always correct
- [ ] No errors or crashes

### ✅ Exit (press 'q')
- [ ] Application exits cleanly
- [ ] No error messages
- [ ] Returns to shell prompt

## Success Criteria
All checkboxes above should be checked ✓

## Notes
- Line numbers from start are positive: 1, 2, 3, ...
- Line numbers from end are negative: -25, -24, ..., -2, -1
- The status bar shows the current viewing mode

## If Issues Found
1. Note which test failed
2. Check the line numbers displayed
3. Check the status bar message
4. Report the issue with details
