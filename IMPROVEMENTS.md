# Improvement Ideas

## Performance

1. **Content cache is write-only — wire up the read path** — The LRU cache (`lcContent`) is populated on every scan but never consulted before scanning. All `getLinesFrom*` functions always re-scan from disk. Adding a lookup-before-scan check would skip `hSeek`/`hGet` syscalls *and* the re-parsing cost (LF splitting, offset calculation, UTF-8 decoding, CR stripping). The main win is avoiding re-parsing — the OS page cache already prevents real disk I/O on revisited regions. Common log viewer patterns that benefit: scroll-down-then-back-up, page-down/page-up, terminal resize (reloads same position). The sparse index provides line-number → offset, and `lcContent` provides offset → Text; both exist, they just need a lookup path before falling through to scan. See `LineCache.lhs` "Offset Source-of-Truth Principle" for the correctness invariant any read-path implementation must preserve.
2. **`updateLRU` is dead code** — defined in LineCache but never called. Once a cache-hit lookup path exists (item 1), it should be called on hits to properly promote accessed entries. Until then, it's unused scaffolding.
3. **LRU list is O(n)** — `insertWithEviction` uses `filter (/= offset)` on a plain list. If the cache is kept and wired up, replace with `Data.Sequence` or a `Map`-based ordered structure for O(log n) operations.
4. **Sparse index granularity is fixed** — adaptive granularity (denser near recently accessed regions) could improve seek latency for interactive use.
5. **`getTotalLines` without full scan** — currently requires forward+backward frontiers to overlap. A background thread could pre-scan to build the index without blocking the UI.

## Robustness

6. **No resource cleanup on exception in `initializeViewer`** — if `getLinesFromStart` succeeds but later operations fail, the cache handle leaks. Use `bracket` or `withLineCache`.
7. **File handle management** — `ensureHandle` reopens on `Nothing` but there's no explicit close-on-modification. If the file is replaced (not just modified), the stale handle could cause issues.
8. **Frontier overlap formula** — `fwdCnt + bwdCnt` assumes no overlap in lines scanned. If both directions scan the same region, total count could be wrong.

## Features

9. **Search/grep** — README lists it as planned. Adding regex or literal search with highlighting would be high-value for a log viewer.
10. **Follow mode (tail -f)** — watch for file appends and auto-scroll to end. Natural for log viewing.
11. **Line wrapping mode** — currently only horizontal scroll; wrapping would help on narrow terminals.
12. **Bookmarks / jump-to-line** — type a line number and jump directly (the sparse index already supports this efficiently).
13. **Status bar shows absolute line numbers** — when scanning from end, display "line N of M" once total is known.
14. **Mouse support** — Brick supports mouse events; scroll wheel would be natural.

## Code Quality

15. **Duplicated BOF/EOF guard logic** — `scrollUp`, `pageUp` both independently check `FromStart && firstLineNum <= 1`. Extract an `isAtBOF :: ViewState -> Bool` helper.
16. **`ViewState` uses a raw record** — could benefit from lenses (already depends on `microlens`) for nested cursor updates.
17. **Test coverage gaps** — no tests for: malformed UTF-8, very large files (stress), concurrent access, partial mmap window edge cases, or Brick rendering output.
18. **No property-based testing** — QuickCheck/Hedgehog for offset invariants (e.g., "round-trip: scan forward then backward from same offset yields same lines").
19. **`Systematic.hs` uses a custom harness** — migrating to HSpec would unify test infrastructure and give better reporting/filtering.

## Tooling / DevEx

20. **No HLint or Fourmolu config** — adding these would enforce consistent style.
21. **`.cabal` is committed but generated** — add `ha-file-viewer.cabal` to `.gitignore` or document "run `stack build` to regenerate."

