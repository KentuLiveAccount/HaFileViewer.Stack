# HaFileViewer.Stack

Stack project for a gigabyte-scale file viewer.

This workspace contains a minimal scaffold:

- Library: `HaFileViewer.Backend` - core file access primitives (random access reads).
- Executable: `ha-file-viewer-cui` - simple character UI pager (app/cui/Main.hs).
- Executable: `ha-file-viewer-web` - small Scotty web server exposing byte ranges (app/web/Main.hs).

Design notes
- The backend currently provides a small API around seek/read. For very large files consider using memory-mapping (mmap) or building an index of line offsets for fast seeks by line number.

How to build

1. Ensure you have Stack installed: https://docs.haskellstack.org
2. From the project root run:

```powershell
stack build
```

How to try the CUI

```powershell
stack exec -- ha-file-viewer-cui -- <path-to-large-file>
```

How to try the webserver

```powershell
stack exec -- ha-file-viewer-web -- --
# then in browser or via curl:
http://localhost:3000/range?path=C:\\path\\to\\file&off=0&len=4096
```

Next steps
- Add mmap-based backend for zero-copy reads.
- Implement line-indexing to support fast jumps to line numbers.
- Improve CUI with proper terminal control (vty/brick) and search.
- Implement webview client that talks to the webserver and renders efficiently.

