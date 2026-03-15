module HaFileViewer.CUILogViewer.ViewState
  ( -- * Types
    ViewState(..)
  , ViewCursor(..)
  , LineWithNumber
    
    -- * Functions
  , shiftViewportDown
  , shiftViewportUp
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

-- | Shift viewport down by removing first line and adding new line at end
shiftViewportDown :: [LineWithNumber] -> LineWithNumber -> Int -> [LineWithNumber]
shiftViewportDown viewport newLine maxSize = 
  take maxSize (drop 1 viewport ++ [newLine])

-- | Shift viewport up by adding new line at start and removing last line
shiftViewportUp :: LineWithNumber -> [LineWithNumber] -> Int -> [LineWithNumber]
shiftViewportUp newLine viewport maxSize =
  take maxSize (newLine : viewport)
