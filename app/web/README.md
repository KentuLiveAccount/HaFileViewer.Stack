# app/web

Small Scotty-based webserver that exposes simple endpoints for file metadata
and byte-range retrieval. Intended to be used by a webview client.

Endpoints
- `GET /open?path=<path>` — returns JSON `{ "size": <bytes> }`
- `GET /range?path=<path>&off=<byte>&len=<bytes>` — returns raw bytes for the
  requested byte range.

Example
```powershell
stack exec -- ha-file-viewer-web -- --
# then in another shell:
curl "http://localhost:3000/range?path=C:\\path\\to\\file.txt&off=0&len=4096" -o out.bin
```

Notes
- The server currently opens the file per request. For production-style
  performance consider a persistent file-handle manager, caching or mmap.
