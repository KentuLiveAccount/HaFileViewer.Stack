# app/cui

Simple character-based pager that reads a file in fixed-size byte pages and
renders them to the terminal.

Usage
```powershell
stack exec -- ha-file-viewer-cui -- "C:\path\to\file.txt"
```

Controls
- `n` — next page
- `p` — previous page
- `q` — quit

Notes
- Page size is currently fixed (4096 bytes). Consider upgrading to a proper
  terminal UI (vty/brick) and making page size configurable.
