module HaFileViewer.CUILogViewer.ViewState
  ( -- * Types
    ViewState(..)
  , ViewCursor(..)
  , ScanOrigin(..)
  , LineWithNumber
    
    -- * Functions (to be implemented in later steps)
  , calculateDisplayLineNumber
  , shiftViewportDown
  , shiftViewportUp
  , updateCursorForward
  , updateCursorBackward
  ) where

import qualified Data.Text as T
import HaFileViewer.LineCache (LineCache)
import HaFileViewer.LineMap.Common (Offset)

-- | Origin point for scanning operations
data ScanOrigin 
  = FromStart  -- ^ Scanning forward from file start
  | FromEnd    -- ^ Scanning backward from file end
  deriving (Show, Eq)

-- | Cursor tracking position in file
data ViewCursor = ViewCursor
  { cursorOffset   :: Offset    -- ^ Current byte position in file
  , cursorLineNum  :: Integer   -- ^ Number of lines read from origin
  , cursorOrigin   :: ScanOrigin  -- ^ Scan direction (forward/backward)
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
  , vsFileSize     :: Integer             -- ^ Size of file in bytes
  }

-- Placeholder implementations (to be completed in later steps)

calculateDisplayLineNumber :: ViewCursor -> Int -> Integer
calculateDisplayLineNumber = undefined

shiftViewportDown :: [LineWithNumber] -> LineWithNumber -> Int -> [LineWithNumber]
shiftViewportDown = undefined

shiftViewportUp :: LineWithNumber -> [LineWithNumber] -> Int -> [LineWithNumber]
shiftViewportUp = undefined

updateCursorForward :: ViewCursor -> [(T.Text, Offset)] -> ViewCursor
updateCursorForward = undefined

updateCursorBackward :: ViewCursor -> [(T.Text, Offset)] -> ViewCursor
updateCursorBackward = undefined
