module HaFileViewer.CUILogViewer.ViewState
  ( -- * Types
    ViewState(..)
  , ViewCursor(..)
  , LineWithNumber
    
    -- * Functions
  , applyLoad
  , applyScrollDown
  , applyScrollUp
  , applyShift
  ) where

import qualified Data.Text as T
import HaFileViewer.Backend.LineCache (LineCache, LinePosition, ScanOrigin(..))

-- | Cursor tracking position in file with two-position tracking for bidirectional scrolling
data ViewCursor = ViewCursor
  { cursorTopPosition    :: LinePosition -- ^ Position at top of viewport (for scrolling up)
  , cursorBottomPosition :: LinePosition -- ^ Position at bottom of viewport (for scrolling down)
  , cursorFirstLine      :: Integer      -- ^ First line number in viewport (e.g., -25)
  , cursorLastLine       :: Integer      -- ^ Last line number in viewport (e.g., -1)
  , cursorOrigin         :: ScanOrigin   -- ^ Scan direction (forward/backward)
  } deriving (Show, Eq)

-- | Line with its display line number
type LineWithNumber = (Integer, T.Text)

-- | Application state for log viewer
data ViewState = ViewState
  { vsCache        :: LineCache           -- ^ LineCache for file access
  , vsCursor       :: ViewCursor          -- ^ Current viewing position
  , vsViewport     :: [LineWithNumber]    -- ^ Currently visible lines
  , vsViewportSize :: Int                 -- ^ Number of lines to display
  , vsFilePath     :: FilePath            -- ^ Path to the file being viewed
  }

-- | Apply a full viewport reload result to ViewState, enforcing size invariant.
-- Derives cursorFirstLine/cursorLastLine from the loaded lines (no manual arithmetic).
-- Returns vs unchanged if lines is empty (EOF/BOF boundary or IO failure).
-- Pass Just origin to override cursorOrigin (e.g. jumpToStart/jumpToEnd).
applyLoad :: Maybe ScanOrigin  -- ^ Override cursor origin, or Nothing to preserve
          -> [LineWithNumber]  -- ^ Lines returned by cache (empty = no-op)
          -> LinePosition      -- ^ Top position (for scrolling up)
          -> LinePosition      -- ^ Bottom position (for scrolling down)
          -> ViewState
          -> ViewState
applyLoad _ [] _ _ vs = vs
applyLoad mOrigin lines topPos botPos vs =
  let loaded = take (vsViewportSize vs) lines
      origin = case mOrigin of
        Just o  -> o
        Nothing -> cursorOrigin (vsCursor vs)
      newCursor = (vsCursor vs)
        { cursorTopPosition    = topPos
        , cursorBottomPosition = botPos
        , cursorFirstLine      = fst (head loaded)
        , cursorLastLine       = fst (last loaded)
        , cursorOrigin         = origin
        }
  in vs { vsViewport = loaded, vsCursor = newCursor }

-- | Apply a single-line shift to ViewState (used by scrollDown/scrollUp).
-- Updates positions and derives new first/last line from the shifted viewport.
applyShift :: [LineWithNumber]  -- ^ New viewport after shift
           -> LinePosition      -- ^ New top position
           -> LinePosition      -- ^ New bottom position
           -> ViewState
           -> ViewState
applyShift newViewport topPos botPos vs =
  let newCursor = (vsCursor vs)
        { cursorTopPosition    = topPos
        , cursorBottomPosition = botPos
        , cursorFirstLine      = if null newViewport then 0 else fst (head newViewport)
        , cursorLastLine       = if null newViewport then 0 else fst (last newViewport)
        }
  in vs { vsViewport = newViewport, vsCursor = newCursor }

-- | Scroll down by one line, handling empty result (EOF) as no-op.
applyScrollDown :: ViewState
                -> ([LineWithNumber], LinePosition, LinePosition)  -- ^ Cache result (empty lines = EOF, no-op)
                -> ViewState
applyScrollDown vs ([], _, _)            = vs
applyScrollDown vs (newLine:_, top, bot) =
  applyShift (shiftViewportDown (vsViewport vs) newLine (vsViewportSize vs)) top bot vs

-- | Scroll up by one line, handling empty result (BOF) as no-op.
applyScrollUp :: ViewState
              -> ([LineWithNumber], LinePosition, LinePosition)  -- ^ Cache result (empty lines = BOF, no-op)
              -> ViewState
applyScrollUp vs ([], _, _)            = vs
applyScrollUp vs (newLine:_, top, bot) =
  applyShift (shiftViewportUp newLine (vsViewport vs) (vsViewportSize vs)) top bot vs

-- Shift viewport down by removing first line and adding new line at end
shiftViewportDown :: [LineWithNumber] -> LineWithNumber -> Int -> [LineWithNumber]
shiftViewportDown viewport newLine maxSize =
  take maxSize (drop 1 viewport ++ [newLine])

-- Shift viewport up by adding new line at start and removing last line
shiftViewportUp :: LineWithNumber -> [LineWithNumber] -> Int -> [LineWithNumber]
shiftViewportUp newLine viewport maxSize =
  take maxSize (newLine : viewport)
