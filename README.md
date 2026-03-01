# HaFileViewer.Stack

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

The library is organized into three layers:

### Layer 1: BidirectionalScanner (Low-Level)
**Module**: `HaFileViewer.BidirectionalScanner`

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
**Module**: `HaFileViewer.LineCache`

Caching layer with sparse index for efficient random access.

**Key Features**:
- LRU eviction policy (configurable cache size)
- Sparse index for fast seeking to any line number
- File modification tracking (timestamp-based)
- Integration with BidirectionalScanner for correct offset tracking

**API**:
```haskell
openLineCache :: FilePath -> IO LineCache
getLines :: LineCache -> Integer -> Int -> IO [T.Text]
getTotalLines :: LineCache -> IO Integer
closeLineCache :: LineCache -> IO ()
```

### Layer 3: LogViewer (High-Level)
**Module**: `HaFileViewer.LogViewer` *(Coming in Phase 2)*

Convenience API for common log viewing operations.

**Planned Features**:
- Simple open/close/with-resource management
- Convenience methods (viewFirstLines, viewLastLines)
- Search functionality
- Cache statistics and introspection

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

# Build and run a test program
stack ghc -- -o test_example test_example.hs
./test_example
```

## Testing

The project has comprehensive test coverage:

- **BidirectionalScanner Tests**: 22 tests covering forward/backward scanning, edge cases, offset tracking
- **LineCache Tests**: 21 tests covering caching, LRU eviction, file modification detection
- **Unit Tests**: 6 tests for `calculatePieceOffsets` helper function
- **Validation Tests**: 5 round-trip tests proving offsets match actual file positions

**Total**: 54 tests, all passing ✅

## Project Structure

```
HaFileViewer.Stack/
├── src/
│   └── HaFileViewer/
│       ├── BidirectionalScanner.lhs    -- Core scanning engine
│       ├── LineCache.lhs                -- Caching and sparse index
│       ├── LineMap/
│       │   └── Common.lhs               -- Shared types (Offset, Direction)
│       └── Internal/
│           └── SparseIndex.lhs          -- Pure sparse index implementation
├── test/
│   ├── BidirectionalScannerTest.lhs     -- Scanner tests
│   └── LineCacheTest.lhs                -- Cache tests
├── app/
│   ├── cui/Main.hs                      -- Character UI pager (planned)
│   └── web/Main.hs                      -- Web server (planned)
├── ha-file-viewer.cabal                 -- Cabal package description
├── package.yaml                         -- Stack package config (Hpack)
├── stack.yaml                           -- Stack configuration
└── README.md                            -- This file
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

See `src/HaFileViewer/BidirectionalScanner.lhs` for detailed comments explaining the implementation.

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

### ✅ Complete (Phase 1)
- BidirectionalScanner with offset tracking
- LineCache with LRU caching
- Sparse index enabled and functional
- 54 tests passing
- File modification detection

### 📋 Planned (Phase 2)
- LogViewer high-level API
- Convenience methods
- Search functionality
- Integration tests

### 🔮 Future Optimizations (Phase 3)
- Statistics tracking (cache hits/misses)
- Performance profiling for large files
- Memory usage optimization
- getTotalLines without full scan

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

### UTF-8 Considerations

- Offsets are **byte positions**, not character positions
- Multi-byte UTF-8 characters (emoji, accents) take multiple bytes
- Never recalculate offsets by encoding Text → ByteString
- Always use byte-level operations (`BS.length`, `BS.split`)

## License

See LICENSE file.

## Contributing

This is currently a learning/development project. Architecture decisions and implementation details are documented in session checkpoints.

## Original Project Goals

This workspace originally scaffolded a gigabyte-scale file viewer with:
- CUI: Character UI pager (`app/cui/Main.hs`)
- Web: Scotty web server with line-oriented endpoints (`app/web/Main.hs`)
  - `/lines` endpoint with negative indexing support

Those components are planned for future integration with the new LineCache/LogViewer architecture.

