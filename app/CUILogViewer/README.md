# CUI Log Viewer

An interactive terminal-based log file viewer built with Haskell and the brick TUI framework.

## Features

- 📜 **Smooth Scrolling**: Navigate through large log files line-by-line or page-by-page
- 🔢 **Smart Line Numbers**: Positive numbers from start (1, 2, 3...), negative from end (-3, -2, -1)
- ⚡ **Efficient Caching**: LRU cache with sparse index for fast random access
- 📍 **Position Tracking**: Always know where you are in the file
- 🎯 **No Memory Limits**: Handles arbitrarily large files (tested with multi-GB files)
- 🛡️ **Error Handling**: Gracefully handles empty files, missing files, and edge cases

## Building

```bash
# From project root
stack build

# Or build and run directly
stack build && stack exec cui-log-viewer <filepath>
```

## Usage

```bash
# Basic usage
stack exec cui-log-viewer <filepath>

# Example
stack exec cui-log-viewer /var/log/app.log
stack exec cui-log-viewer test-sample.txt
```

### What You'll See

```
┌─ CUI Log Viewer ─────────────────────────────────────────────┐
│    1: [2024-01-01 10:00:00] Application started              │
│    2: [2024-01-01 10:00:01] Loading configuration            │
│    3: [2024-01-01 10:00:02] Connecting to database           │
│   ...                                                         │
│   25: [2024-01-01 10:00:24] Ready to accept connections      │
├───────────────────────────────────────────────────────────────┤
│ File: /var/log/app.log  |  Lines: 1+...  |  [START]         │
│ q:quit g:top G:end ↑↓:scroll PgUp/Dn:page                    │
└───────────────────────────────────────────────────────────────┘
```

## Key Bindings

### Navigation

| Key(s)        | Action                              | Description                           |
|---------------|-------------------------------------|---------------------------------------|
| `↓` or `j`    | Scroll down one line                | Move forward through file             |
| `↑` or `k`    | Scroll up one line                  | Move backward through file            |
| `PgDn`        | Scroll down one page                | Jump forward ~25 lines                |
| `PgUp`        | Scroll up one page                  | Jump backward ~25 lines               |
| `Home` or `g` | Jump to start                       | Go to beginning of file (line 1)      |
| `End` or `G`  | Jump to end                         | Go to end of file (line -1)           |
| `q` or `Esc`  | Quit                                | Exit the viewer                       |

### Line Numbering

**From Start (Forward Navigation):**
- Lines are numbered positively: `1, 2, 3, 4, ...`
- Status bar shows: `Lines: 1+...`

**From End (Backward Navigation):**
- Lines are numbered negatively: `-25, -24, -23, ... -2, -1`
- Status bar shows: `Lines: ...-25`
- Last line of file is always `-1`

**Why negative numbers?**
- No need to count total lines (expensive for huge files)
- Intuitive: `-1` means "last line", `-10` means "10th from last"
- Lazy evaluation: only read what you need

### Position Indicators

Status bar shows current position:
- `[START]` - At beginning of file (line 1)
- `[END]` - At end of file (last line)
- `Lines: 1+...` - Viewing from start, currently at line 1+
- `Lines: ...-25` - Viewing from end, currently showing last 25 lines

## Examples

### Viewing System Logs

```bash
# View nginx access log
stack exec cui-log-viewer /var/log/nginx/access.log

# Navigate to end to see most recent entries
# Press 'G' (Shift+g) to jump to end
# Use ↑/↓ to scroll through recent logs
```

### Inspecting Application Logs

```bash
# Open large application log
stack exec cui-log-viewer app.log

# Start at beginning (default)
# Press PgDn repeatedly to skip through
# Press 'G' to jump to end
# Press 'g' to return to start
```

### Quick File Inspection

```bash
# Check file contents quickly
stack exec cui-log-viewer data.txt

# Jump to end with 'G' to see last lines
# Quit immediately with 'q'
```

## Architecture

CUILogViewer is built on top of a three-layer architecture:

```
┌─────────────────────────────┐
│  CUI Log Viewer (brick TUI) │ ← Interactive terminal UI
├─────────────────────────────┤
│  LineCache                  │ ← LRU caching + sparse index
├─────────────────────────────┤
│  BidirectionalScanner       │ ← Efficient forward/backward scanning
└─────────────────────────────┘
```

- **BidirectionalScanner**: Reads lines forward or backward with offset tracking
- **LineCache**: Maintains LRU cache and sparse index for fast seeking
- **CUI Log Viewer**: Interactive TUI with keyboard navigation

## Technical Details

### Offset-Based Cursor

Instead of tracking line numbers, CUILogViewer tracks **byte offsets** in the file:
- More efficient (no need to count all lines)
- Works with LineCache's offset-based API
- Enables lazy evaluation (only read what's visible)

### Pure Functions

Core logic is implemented as pure, testable functions:
- `calculateDisplayLineNumber`: Convert offset position to display line number
- `shiftViewportDown/Up`: Update visible lines
- `updateCursorForward/Backward`: Track position after reading

### Performance

- **Memory**: O(viewport size) - only loads visible lines
- **Random Access**: O(log n) with sparse index
- **Sequential Reading**: O(n) - optimal for large files
- **Cache**: LRU eviction keeps memory bounded

## Limitations & Known Issues

### Current Limitations

1. **No Search**: Cannot search for patterns (planned for Phase 3)
2. **No Filtering**: Cannot filter lines by keyword (planned for Phase 3)
3. **Approximate Scrolling**: Line-by-line scrolling uses byte approximations
   - Works correctly but may occasionally skip/repeat lines
   - Page scrolling is more accurate

### Edge Cases

- **Empty Files**: Shows "Error: File is empty"
- **Missing Files**: Shows "Error: File not found: <path>"
- **Single-Line Files**: Displays correctly
- **Very Long Lines**: May wrap or truncate depending on terminal width

## Future Enhancements (Phase 3)

Planned features for future releases:

- **Search**: `/` to search forward, `n` for next match
- **Filtering**: Show only lines matching a pattern
- **Syntax Highlighting**: Colorize log levels (ERROR, WARN, INFO)
- **Follow Mode**: Auto-scroll as file grows (`tail -f` behavior)
- **Bookmarks**: Mark interesting lines and jump between them
- **Export**: Save filtered/searched results to new file

## Troubleshooting

### App Won't Build

```bash
# Make sure stack is up to date
stack upgrade

# Clean and rebuild
stack clean
stack build
```

### App Crashes on Large Files

The app should handle large files gracefully. If you encounter crashes:
1. Check available memory
2. Try reducing cache size (requires code modification)
3. Report issue with file size and error message

### Keyboard Doesn't Work

- Make sure terminal supports key events
- Try alternative keys (j/k instead of ↑↓)
- Check terminal emulator compatibility with brick/vty

### Line Numbers Are Wrong

- Line numbers are **relative** to where you started viewing
- Press `g` to reset to line 1
- Press `G` to view from end (negative numbers)

## Development

### Running Tests

```bash
# Run all tests
stack test

# Run specific test suite
stack test ha-file-viewer:test:bidirectional-scanner-test
stack test ha-file-viewer:test:linecache-test

# Run CUI unit tests manually
stack ghc -- -o test_step3 test_step3_lineNumbers.hs
./test_step3.exe
```

### Project Structure

```
app/CUILogViewer/
├── Main.hs                        # Main entry point, brick app
└── README.md                      # This file

src/HaFileViewer/
├── CUILogViewer/
│   └── ViewState.hs              # Pure functions, types
├── LineCache.lhs                 # Caching layer
└── BidirectionalScanner.lhs      # Core scanning engine

test_step*.hs                     # Unit tests for pure functions
```

## Credits

Built with:
- [brick](https://github.com/jtdaugherty/brick) - Terminal UI framework
- [vty](https://github.com/jtdaugherty/vty) - Terminal rendering
- [Stack](https://docs.haskellstack.org) - Haskell build tool

## License

See LICENSE file in project root.

---

**Quick Start:**
```bash
stack build && stack exec cui-log-viewer /var/log/syslog
```

**Keys to Remember:**
- `↑↓` or `jk` to scroll
- `PgUp/PgDn` to jump pages  
- `g` for start, `G` for end
- `q` to quit

Happy log viewing! 📜✨
