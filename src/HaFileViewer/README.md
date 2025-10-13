# src/HaFileViewer

This package contains the core backend library for HaFileViewer.

Public API (module `HaFileViewer.Backend`)
- `openFileHandle :: FilePath -> IO FileHandle` — open a file for random access.
- `closeFileHandle :: FileHandle -> IO ()` — close the handle.
- `readChunkAt :: FileHandle -> Integer -> Int -> IO ByteString` — read up to N bytes at a byte offset.
- `fileSize :: FileHandle -> Integer` — get file size in bytes.

Notes
- The implementation currently uses Seek + hGet. For best performance on
  gigabyte-scale files consider using mmap or building a line-offset index.
