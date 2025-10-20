# app/web

Small Scotty-based webserver that exposes simple endpoints for file metadata
and byte-range retrieval. Intended to be used by a webview client.

Endpoints
- `GET /open?path=<path>` — returns JSON `{ "size": <bytes> }`
- `GET /range?path=<path>&off=<byte>&len=<bytes>` — returns raw bytes for the requested byte range.
- `GET /lines?path=<path>&start=<int>&count=<int>` — returns JSON with requested lines. `start` may be negative to index from the end (e.g. `start=-1` returns the last line as start).

Behavior notes
- The server currently maintains a single in-memory LineMap cache for the last-opened file to provide fast line-based access for one session. This is stateful (single-instance) and optimized for a single client session. If you need stateless scaling, consider running a single instance or adding index persistence/shared store.

Example
```powershell
stack exec -- ha-file-viewer-web -- --
# then in another shell:
curl "http://localhost:3000/range?path=C:\\path\\to\\file.txt&off=0&len=4096" -o out.bin
```

Notes
- The server currently opens the file per request. For production-style
  performance consider a persistent file-handle manager, caching or mmap.
