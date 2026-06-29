# HaFileViewer.Stack

[![CI](https://github.com/KentuLiveAccount/HaFileViewer.Stack/actions/workflows/ci.yml/badge.svg)](https://github.com/KentuLiveAccount/HaFileViewer.Stack/actions/workflows/ci.yml)

A high-performance log file viewer library written in Haskell, designed for efficient viewing of large log files with bidirectional scanning and intelligent caching.

## Features

- **Bidirectional Scanning**: Read lines forward or backward with symmetric API
- **Efficient Caching**: LRU cache with configurable size and eviction
- **Sparse Index**: Fast random access to any line in large files (O(log n))
- **Offset Tracking**: Precise byte-level offset tracking during scanning
- **Large File Support**: Chunk-based reading handles arbitrarily large files
- **Platform Independent**: Works on both Unix (LF) and Windows (CRLF) line endings
- **File Modification Detection**: Automatically invalidates cache when file changes

## Architecture

The library is organized into two layers, with a Brick-based terminal UI on top:

### Layer 1: BidirectionalScanner (Low-Level)
**Module**: `HaFileViewer.Backend.BidirectionalScanner`

Core scanning engine that reads lines from files in either direction.

**Key Features**:
- Strategy pattern for direction-dependent operations
- Chunk-based reading with partial line handling
- Canonical format (treats all files as if they end with LF)
- Offset tracking during scanning (not recalculated afterward)

**API**:
```haskell
scanLines :: Direction -> Integer -> (Offset -> Integer -> IO BS.ByteString) -> Int -> IO [T.Text]
scanLinesWithOffsets :: Direction -> Integer -> (Offset -> Integer -> IO BS.ByteString) -> Int -> IO [(T.Text, Offset)]
```

### Layer 2: LineCache (Mid-Level)
**Module**: `HaFileViewer.Backend.LineCache`

Caching layer with LRU content cache and sparse line-number index.

**Key Features**:
- LRU content cache keyed by byte offset, with chain-based hit lookup
- Sparse index for fast seeking to any line number
- File modification tracking (timestamp-based)
- Integration with BidirectionalScanner for correct offset tracking

**API**:
```haskell
openLineCache    :: FilePath -> IO LineCache
withLineCache    :: FilePath -> (LineCache -> IO a) -> IO a
getLinesFromStart :: LineCache -> Int -> IO GetLinesResult
getLinesFromEnd   :: LineCache -> Int -> IO GetLinesResult
getLinesFrom      :: LineCache -> LinePosition -> Direction -> Int -> Integer -> IO GetLinesResult
getCacheStats     :: LineCache -> IO CacheStats
closeLineCache    :: LineCache -> IO ()
```

### Terminal UI: CUILogViewer
**Modules**: `HaFileViewer.CUILogViewer.{Main,Operations,ViewState}`

A Brick/Vty pager that consumes the `LineCache` API directly — no
intermediate convenience layer. `Operations` exposes scroll/jump/resize
actions; `ViewState` holds the pure cursor state; `Main` wires Brick
events.

## Building

This project uses [Stack](https://docs.haskellstack.org/):

```bash
# Build the project
stack build

# Run tests
stack test

# Run specific test suite
stack test ha-file-viewer:test:bidirectional-scanner-test
stack test ha-file-viewer:test:linecache-test
stack test ha-file-viewer:test:ui-systematic-test

# Run the terminal pager on a file
stack run cui-log-viewer -- <filepath>
```

## Testing

The project has three test suites, run via `stack test`:

- **BidirectionalScanner**: 22 HSpec examples covering forward/backward scanning, edge cases, and offset tracking.
- **LineCache**: 18 HSpec examples covering CR-LF handling, incremental scrolling, file modification, error propagation, and the cache hit-path round-trip equivalence.
- **CUI systematic**: 42 custom-harness tests covering viewport state, scroll/page/jump operations, reversibility, boundary conditions, and empty-file/error paths.

**Total**: 82 tests, all passing ✅

## Project Structure

```
HaFileViewer.Stack/
├── src/HaFileViewer/
│   ├── Backend/
│   │   ├── Types.lhs                       -- Shared types (Offset) + mmap helpers
│   │   ├── BidirectionalScanner.lhs        -- Core scanning engine
│   │   ├── SparseIndex.lhs                 -- Pure sparse index
│   │   ├── LineCache.lhs                   -- LRU content cache + sparse index integration
│   │   └── Test/
│   │       ├── BidirectionalScanner.hs     -- Scanner HSpec tests
│   │       └── LineCache.hs                -- Cache HSpec tests
│   └── CUILogViewer/
│       ├── Main.hs                         -- Brick/Vty entry point
│       ├── Operations.hs                   -- Scroll/page/jump/resize actions
│       ├── ViewState.hs                    -- Pure viewport state
│       └── Test/Systematic.hs              -- Custom-harness UI tests
├── ha-file-viewer.cabal                    -- Generated from package.yaml
├── package.yaml                            -- Stack package config (Hpack, source of truth)
├── stack.yaml                              -- Stack configuration
└── README.md                               -- This file
```

## Implementation Highlights

### Offset Tracking (Critical Design Decision)

The library tracks byte offsets **during** scanning, not afterward. This is critical because:

1. **Information Loss**: Offsets are lost during `BS.split` operations
2. **Partial Lines**: Partial lines carry offsets between chunks
3. **UTF-8 Encoding**: Re-encoding Text → ByteString gives wrong byte counts
4. **Backward Scans**: Must calculate offsets from chunk start, not end

**Key Implementation Details**:
- `ScanState` has `ssLineOffsets` parallel to `ssLines`
- `calculatePieceOffsets` computes byte offsets for each piece after splitting
- `extractLinesCanonical` preserves partial line offsets across chunks
- `processChunk` correctly handles backward scan offset calculation (subtracts chunk size)

See `src/HaFileViewer/Backend/BidirectionalScanner.lhs` for detailed comments explaining the implementation.

### Sparse Index

The sparse index stores every Nth line's offset (configurable granularity). This enables:
- **Fast Seeking**: Binary search to find nearest indexed offset (O(log n))
- **Bounded Memory**: Memory usage = total_lines / granularity
- **Incremental Building**: Index grows as file is scanned

Example: For a 1M line file with granularity 1000:
- Stores 1000 index entries
- Memory: ~16KB for index
- Random access: ~10-100x faster than linear scan

## Literate Programming

Source files use `.lhs` (Literate Haskell) format with Bird-style notation (`>`). This allows mixing code with comprehensive documentation.

## Current Status

### ✅ Shipped
- `BidirectionalScanner` with in-scan offset tracking (forward + backward)
- `LineCache` with offset-keyed LRU content cache and chain-based hit lookup
- Sparse line-number index (write side; read side wired up alongside jump-to-line)
- File modification detection and cache invalidation
- Brick/Vty terminal pager (`cui-log-viewer`) backed by the cache
- 82 tests across three suites

### 🗺 Roadmap

Concrete next features and improvements live in [IMPROVEMENTS.md](./IMPROVEMENTS.md).
The shortlist:

- **Search / grep** with line-number-anchored results
- **Follow mode** (`tail -f` style append-watching for live logs)
- **Jump-to-line by absolute line number** (the read path that activates `SparseIndex.lookupNearest`)
- **Mouse / scroll-wheel support** in the TUI
- **Line wrapping** for narrow terminals

### 🔮 Future direction: webview UI over local HTTP

The longer-term plan is a second UI: a webview front-end talking to a
local HTTP server that exposes the `LineCache` API. The current
terminal pager and the future webview would both consume the same
backend, with the local HTTP layer being the shared boundary. Nothing
in the current code commits to this design — it's listed here as
direction, not promise.

## Technical Notes

### Backward Scan Offset Bug (Fixed)

**Problem**: Backward scans were calculating piece offsets from the wrong position (chunk end instead of chunk start).

**Solution**: 
```haskell
chunkStartOffset = case stratPartialSide strat of
  LeftPartial  -> ssOffset state               -- Forward: ssOffset is chunk start
  RightPartial -> ssOffset state - offsetDelta -- Backward: subtract to get start
```

This was caught by round-trip validation tests that verify offsets match actual file positions.

### Partial Line Offset Preservation

When a line spans multiple chunks, the FIRST chunk determines its offset:

```haskell
-- Example: "hello" at offset 100, next chunk "world\n"
-- Combined line "helloworld" MUST have offset 100 (not offset of "world")
edgeLineOffset = partialOffset  -- Use partial's offset, not edge's!
```

### CR-LF Line Ending Bug (Fixed)

**Problem**: The `bottomOffset` calculation in LineCache used `+1` for all line endings, assuming Unix-style LF (`\n`). However, Windows files use CR-LF (`\r\n`), which requires 2 bytes.

**Incorrect formula:**
```haskell
bottomOffset = offset + BS.length(text) + 1  -- Always adds 1 byte
```

**Impact**: On CR-LF files, this caused incremental scrolling to show alternating empty lines because the offset pointer would land inside the `\r\n` sequence instead of at the next line's start.

**Solution**: Auto-detect line ending style on file open (scan first chunk for `\r\n` vs `\n`), store in LineCache state, and use the appropriate increment:
```haskell
lineEndingSize = 2  -- for CR-LF files
lineEndingSize = 1  -- for LF-only files
bottomOffset = offset + BS.length(text) + lineEndingSize
```

This ensures offset calculations match the actual file byte layout.

### UTF-8 Considerations

- Offsets are **byte positions**, not character positions
- Multi-byte UTF-8 characters (emoji, accents) take multiple bytes
- Never recalculate offsets by encoding Text → ByteString
- Always use byte-level operations (`BS.length`, `BS.split`)

## License

See LICENSE file.

## Contributing

This is currently a learning/development project. Architecture decisions
and implementation details are documented in checkpoint sessions and in
the source comments of `LineCache.lhs` and `BidirectionalScanner.lhs`.

