# HaFileViewer.Stack

Stack project for a gigabyte-scale file viewer.

This workspace contains a minimal scaffold:

-- Library: `HaFileViewer.LineMap` - line-oriented access and sparse indexing prototype.
-- Executable: `ha-file-viewer-cui` - simple character UI pager (app/cui/Main.hs).
-- Executable: `ha-file-viewer-web` - small Scotty web server exposing line-oriented endpoints (app/web/Main.hs).
	- New: `/lines` endpoint provides line-oriented access (supports negative indexing from end).

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
stack exec -- ha-file-viewer-cui path-to-large-file>
```

How to try the webserver

```powershell
stack exec -- ha-file-viewer-web -- --
# metadata and lines endpoints:
http://localhost:3000/open?path=C:\\path\\to\\file.txt
# lines:
http://localhost:3000/lines?path=C:\\path\\to\\file.txt&start=0&count=100
http://localhost:3000/lines?path=C:\\path\\to\\file.txt&start=-10&count=10  # last 10 lines
```

Next steps
- Add mmap-based backend for zero-copy reads.
- Implement line-indexing to support fast jumps to line numbers.
- Improve CUI with proper terminal control (vty/brick) and search.
-- Implement webview client that talks to the webserver and renders efficiently.

