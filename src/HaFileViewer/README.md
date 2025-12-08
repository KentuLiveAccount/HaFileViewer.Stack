# src/HaFileViewer

- This package contains the core line-oriented library for HaFileViewer.

- Line-oriented API (module `HaFileViewer.LineMap`)
- `openLineMap :: FilePath -> Int -> IO LineMap` — open a file with a sparse line index (index step).
- `closeLineMap :: LineMap -> IO ()` — close the LineMap and underlying file handle.
- `getLines :: LineMap -> Integer -> Int -> IO [Text]` — fetch up to N lines starting at the 0-based line index; `start` may be negative to index from the end (e.g. `-1` = last line).

Notes
- `LineMap` currently uses an in-memory sparse/index-lite approach and is optimized for a single session. It is a prototype that can be extended to persistent sidecars or mmap-backed scanning for higher performance.

Notes
- The implementation currently uses Seek + hGet. For best performance on
  gigabyte-scale files consider using mmap or building a line-offset index.
