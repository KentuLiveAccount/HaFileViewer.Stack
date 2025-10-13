# app

This folder contains the two front-end applications that consume the
`ha-file-viewer` library.

Contents
- `cui/` - Character-based user interface (CLI pager).
- `web/` - Scotty-based webserver used by a webview client.

How to run (from project root)
- CUI: `stack exec -- ha-file-viewer-cui -- <path-to-file>`
- Webserver: `stack exec -- ha-file-viewer-web -- --` (listens on port 3000)

See the READMEs inside each subfolder for more details.
