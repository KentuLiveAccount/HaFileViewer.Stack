module HaFileViewer.CUILogViewer.ViewState
  ( -- * Types
    ViewState(..)
  , ViewCursor(..)
  , ScanOrigin(..)
  , LineWithNumber
    
    -- * Functions
  , calculateDisplayLineNumber
  , shiftViewportDown
  , shiftViewportUp
  ) where

import qualified Data.Text as T
import HaFileViewer.LineCache (LineCache, LinePosition)

-- | Origin point for scanning operations
data ScanOrigin 
  = FromStart  -- ^ Scanning forward from file start
  | FromEnd    -- ^ Scanning backward from file end
  deriving (Show, Eq)

-- | Cursor tracking position in file
data ViewCursor = ViewCursor
  { cursorPosition :: LinePosition -- ^ Opaque position marker in file
  , cursorLineNum  :: Integer      -- ^ Number of lines read from origin
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

-- | Calculate display line number based on cursor position and relative index
-- For FromStart: positive line numbers (1-based)
-- For FromEnd: negative line numbers (-1 is last line)
-- When cursorLineNum=25 (showing 25 lines from end), indices [0..24]:
--   Index 0 (top) → -(25 - 0) = -25 (25th from last) ✓
--   Index 24 (bottom) → -(25 - 24) = -1 (last line) ✓
calculateDisplayLineNumber :: ViewCursor -> Int -> Integer
calculateDisplayLineNumber cursor relativeIndex = 
  case cursorOrigin cursor of
    FromStart -> cursorLineNum cursor + fromIntegral relativeIndex + 1
    FromEnd   -> negate (cursorLineNum cursor - fromIntegral relativeIndex)

-- | Shift viewport down by removing first line and adding new line at end
shiftViewportDown :: [LineWithNumber] -> LineWithNumber -> Int -> [LineWithNumber]
shiftViewportDown viewport newLine maxSize = 
  take maxSize (drop 1 viewport ++ [newLine])

-- | Shift viewport up by adding new line at start and removing last line
shiftViewportUp :: LineWithNumber -> [LineWithNumber] -> Int -> [LineWithNumber]
shiftViewportUp newLine viewport maxSize =
  take maxSize (newLine : viewport)
