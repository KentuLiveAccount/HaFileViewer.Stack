# Copilot Instructions for HaFileViewer.Stack

## Build & Test

```bash
# Build
stack build

# Run all tests
stack test

# Run individual test suites
stack test ha-file-viewer:test:bidirectional-scanner-test
stack test ha-file-viewer:test:linecache-test
stack test ha-file-viewer:test:ui-systematic-test

# Run the CUI pager
stack run cui-log-viewer -- <filepath>
```

GHC options include `-Wall`; fix all warnings.

## Architecture

Three-layer library with a Brick TUI application on top:

1. **Backend.Types** — Shared primitives: `Offset` (= `Integer`), `lfByte`, `normalizeLine`, `decodeUtf8Lenient`, and windowed mmap helpers (`ensureMapped`/`readAtOffset`).
2. **Backend.BidirectionalScanner** — Low-level chunk-based line scanner. Reads forward or backward using a strategy pattern. Tracks byte offsets *during* scanning (never recalculated after).
3. **Backend.SparseIndex** — Thin `Data.Map.Strict` wrapper mapping line numbers → byte offsets. Key primitive: `lookupNearest` (uses `Map.lookupLE`).
4. **Backend.LineCache** — Mid-level caching layer with LRU eviction and sparse index. Handles file-modification detection, frontier tracking, and total-line-count inference.
5. **CUILogViewer** — Brick-based terminal pager. `ViewState` is the pure state; `Operations` exposes scroll/jump actions; `Main` wires Brick events.

Data flows: `CUILogViewer.Operations` → `LineCache.getLinesFrom*` → `BidirectionalScanner.scanLinesWithOffsets` → windowed mmap I/O via `readAtOffset`.

## Internal Design Details

### BidirectionalScanner

- **Strategy pattern**: `ScanStrategy` record encapsulates all direction-dependent logic (piece ordering, partial side, offset math, canonicalization). Forward and backward aren't separate algorithms — they share the same scan loop with swapped strategy fields.
- **ScanState** threads: `ssOffset`, `ssPartial`/`ssPartialOffset`, `ssLines`/`ssLineOffsets` (parallel lists), `ssLineCount`, `ssFileSize`, `ssEndsWithLF`.
- **Chunk pipeline**: read → canonicalize (add virtual LF if missing) → `BS.split lfByte` → `calculatePieceOffsets` → reorder by strategy → `extractLinesCanonical` (merge edge+partial, emit middle, carry new partial) → accumulate.
- **`calculatePieceOffsets`**: left-to-right fold starting at `startOffset`; each piece's offset is current position, next = current + `BS.length piece` + 1 (the +1 is the stripped LF delimiter).
- **Backward offset correction**: after a backward read, `processChunk` subtracts the chunk length from `ssOffset` to get `chunkStartOffset`. This was a past bug; do not refactor without understanding this.
- **Partial-line offset preservation**: when a line spans chunks, the *first* chunk's offset is kept (stored in `ssPartialOffset`, used as `edgeLineOffset`).
- **Return type**: `([(Text, Offset)], Offset)` — the second element is the continuation offset for subsequent scans.

### LineCache

- **Mutable state via IORefs**: `LineCache` is a record of `IORef`s (mod time, file handle, sparse index, content map, LRU order, total-lines, forward/backward scan frontiers).
- **LRU cache**: `Map Offset Text` + `[Offset]` order list. Evicts head of LRU when full and key is new; accessed keys move to tail.
- **Sparse index integration**: after each scan, `SI.insertBatch` records every `ccIndexStep`th line-number → offset pair.
- **Frontier tracking**: forward/backward scans update `lcForwardHighOff`/`lcBackwardLowOff` and their line counts. When frontiers overlap, `checkFrontierOverlap` sets `lcTotalLines`.
- **File modification**: `checkModified` compares cached `UTCTime` vs `getModificationTime`; on change, cache and index are invalidated.
- **Error handling**: all public API functions (`getLinesFromStart`, `getLinesFromEnd`, `getLinesFrom`) wrap IO in `try`; exceptions → `LoadFailed msg`, empty results → `AtBoundary`.
- **`GetLinesResult`**: `LinesLoaded [(Integer, Text)] LinePosition LinePosition | AtBoundary | LoadFailed String`. The two `LinePosition`s are top/bottom continuation points for the CUI layer.
- **Note**: LineCache does NOT use mmap itself; it uses `openBinaryFile` + `hSeek` + `BS.hGet`. The mmap helpers in `Types.lhs` are used by the scanner's `readAtOffset` callback passed in from LineCache.

### CUILogViewer

- **Two-anchor cursor model**: `ViewCursor` tracks both `cursorTopPosition` (for scrolling up) and `cursorBottomPosition` (for scrolling down), plus visible line bounds and `cursorOrigin :: ScanOrigin`.
- **Pure state transforms**: `applyLoad`, `applyScrollDown`, `applyScrollUp`, `applyShift` — all in `ViewState.hs`. They handle `AtBoundary` (no-op), `LoadFailed` (store error), and `LinesLoaded` (update viewport).
- **Scroll operations** (in `Operations.hs`): `scrollDown`/`scrollUp` fetch 1 line; `pageDown`/`pageUp` fetch `vsViewportSize` lines; `jumpToStart`/`jumpToEnd` reload from BOF/EOF.
- **BOF/EOF guards**: `scrollUp` stops when `FromStart && firstLineNum <= 1`; `scrollDown` stops when `FromEnd && cursorLastLine == -1`.
- **Resize handling**: `resizeViewport` reloads from current top position, choosing direction by `cursorOrigin`.
- **Tab expansion**: `expandTabs` converts `\t` to spaces aligned to configurable tab stops, column-aware.
- **Horizontal scroll**: `T.drop vsHScrollOffset` applied after tab expansion.
- **Key bindings**: `q`/`Esc` quit; `↓`/`j` down; `↑`/`k` up; `PgDn`/`PgUp` page; `Home`/`g` start; `End`/`G` end; `←`/`h` and `→`/`l` horizontal; `0` reset horizontal.
- **Event drain**: after scroll ops, buffered Vty events are drained to reduce input lag.
- **Empty-file initialization**: hard `error` — caller must ensure file is non-empty.

## Key Conventions

- **Literate Haskell** — Backend modules use `.lhs` with Bird-style (`>`) notation. CUILogViewer modules use plain `.hs`.
- **Offset invariant** — Byte offsets are tracked *during* `scanLinesWithOffsets`, never reconstructed from Text. Do not re-encode Text → ByteString for offset math.
- **Canonical format** — All files are treated as ending with LF. CR is stripped via `normalizeLine` (`T.dropWhileEnd (== '\r')`).
- **LinePosition is opaque** — Constructors not exported from `LineCache`; consumers use `getLinesFrom*` and pattern-match `GetLinesResult`.
- **Package config** — `package.yaml` (Hpack) is the source of truth; `ha-file-viewer.cabal` is generated. Edit `package.yaml`, not `.cabal`.
- **Module paths** — Exposed modules: `HaFileViewer.Backend.*` and `HaFileViewer.CUILogViewer.*`. Tests: `src/HaFileViewer/Backend/Test/` and `src/HaFileViewer/CUILogViewer/Test/`.
- **Snapshot** — Stackage LTS 24.15 (GHC 9.8). Dependencies pinned via `stack.yaml.lock`.
- **UTF-8 handling** — `decodeUtf8Lenient` replaces invalid bytes; never crash on malformed input.
- **Windowed mmap** — `ensureMapped` maintains an `IORef (Offset, ByteString)` window; remaps centered on the requested offset when access falls outside current window.

## Testing Patterns

- **Backend tests** (HSpec): temp files via `withSystemTempFile`; handles explicitly closed before test action. `readFromFile` helper uses `mmapFileByteString` for test reads.
- **UI systematic tests** (custom harness): `runTest :: String -> IO Bool -> IO ()` prints `[PASS]/[FAIL]`. Tests call real `Operations` functions against real files, then assert on `ViewState` fields.
- **Fixture pattern**: tests create temp files with exact byte content (explicit `\r\n` vs `\n`), run operations, then assert on returned lines/offsets.
- **`unwrap` helper** in LineCache tests: extracts lines from `GetLinesResult` or fails the test on `AtBoundary`/`LoadFailed`.
- **Cleanup**: `withSystemTempFile` for auto-cleanup; explicit `removeFile` in error-path tests (deleted-file scenarios).
