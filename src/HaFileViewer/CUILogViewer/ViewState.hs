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
import HaFileViewer.LineCache (LineCache, LinePosition, ScanOrigin(..))

-- | Cursor tracking position in file
data ViewCursor = ViewCursor
  { cursorPosition :: LinePosition -- ^ Opaque position marker in file
  , cursorOrigin   :: ScanOrigin   -- ^ Scan direction (forward/backward)
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
