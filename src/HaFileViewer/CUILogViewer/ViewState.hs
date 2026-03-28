module HaFileViewer.CUILogViewer.ViewState
  ( -- * Types
    ViewState(..)
  , ViewCursor(..)
  , LineWithNumber
    
    -- * Re-exports from LineCache
  , GetLinesResult(..)
    
    -- * Functions
  , applyLoad
  , applyScrollDown
  , applyScrollUp
  , applyShift
  ) where

import qualified Data.Text as T
import HaFileViewer.Backend.LineCache (LineCache, LinePosition, ScanOrigin(..), GetLinesResult(..))

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
  , vsError        :: Maybe String        -- ^ Last IO error, if any
  }

-- | Apply a full viewport reload result to ViewState, enforcing size invariant.
-- Derives cursorFirstLine/cursorLastLine from the loaded lines (no manual arithmetic).
-- Returns vs unchanged if lines is empty (EOF/BOF boundary or IO failure).
-- Pass Just origin to override cursorOrigin (e.g. jumpToStart/jumpToEnd).
applyLoad :: Maybe ScanOrigin  -- ^ Override cursor origin, or Nothing to preserve
          -> ViewState
          -> GetLinesResult
          -> ViewState
applyLoad _ vs AtBoundary = vs { vsError = Nothing }
applyLoad _ vs (LoadFailed msg) = vs { vsError = Just msg }
applyLoad mOrigin vs (LinesLoaded lines topPos botPos) =
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
  in vs { vsViewport = loaded, vsCursor = newCursor, vsError = Nothing }

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
                -> GetLinesResult
                -> ViewState
applyScrollDown vs AtBoundary            = vs { vsError = Nothing }
applyScrollDown vs (LoadFailed msg)      = vs { vsError = Just msg }
applyScrollDown vs (LinesLoaded [] _ _)  = vs { vsError = Nothing }
applyScrollDown vs (LinesLoaded (newLine:_) top bot) =
  applyShift (shiftViewportDown (vsViewport vs) newLine (vsViewportSize vs)) top bot vs

-- | Scroll up by one line, handling empty result (BOF) as no-op.
applyScrollUp :: ViewState
              -> GetLinesResult
              -> ViewState
applyScrollUp vs AtBoundary            = vs { vsError = Nothing }
applyScrollUp vs (LoadFailed msg)      = vs { vsError = Just msg }
applyScrollUp vs (LinesLoaded [] _ _)  = vs { vsError = Nothing }
applyScrollUp vs (LinesLoaded (newLine:_) top bot) =
  applyShift (shiftViewportUp newLine (vsViewport vs) (vsViewportSize vs)) top bot vs

-- Shift viewport down by removing first line and adding new line at end
shiftViewportDown :: [LineWithNumber] -> LineWithNumber -> Int -> [LineWithNumber]
shiftViewportDown viewport newLine maxSize =
  take maxSize (drop 1 viewport ++ [newLine])

-- Shift viewport up by adding new line at start and removing last line
shiftViewportUp :: LineWithNumber -> [LineWithNumber] -> Int -> [LineWithNumber]
shiftViewportUp newLine viewport maxSize =
  take maxSize (newLine : viewport)
