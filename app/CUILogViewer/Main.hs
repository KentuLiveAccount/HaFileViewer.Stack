{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Main (App(..), defaultMain, halt)
import Brick.Types (BrickEvent(..), EventM)
import Brick.Widgets.Border (hBorder, hBorderWithLabel)
import qualified Graphics.Vty as V
import qualified Data.Text as T
import HaFileViewer.CUILogViewer.ViewState
import HaFileViewer.LineCache
import System.Environment (getArgs)
import System.Directory (getFileSize)

-- Name type for brick
data Name = ViewportName deriving (Ord, Show, Eq)

-- Draw the UI
drawUI :: ViewState -> [Widget Name]
drawUI vs = [viewport]
  where
    -- Render each line with line number
    lineWidgets = map renderLine (vsViewport vs)
    renderLine (lineNum, text) = 
      hBox [ padLeft (Pad 1) $ str (show lineNum)
           , str ": "
           , txt text
           ]
    
    -- Status bar
    statusBar = hBox
      [ str "File: "
      , str (vsFilePath vs)
      , str "  |  "
      , str "Lines: "
      , str (show (length (vsViewport vs)))
      , str "  |  "
      , str "Press 'q' to quit"
      ]
    
    -- Full viewport
    viewport = vBox
      [ hBorderWithLabel (str " CUI Log Viewer ")
      , vBox lineWidgets
      , hBorder
      , statusBar
      ]

-- Handle keyboard events
handleEvent :: BrickEvent Name e -> EventM Name ViewState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ = return ()

-- Brick app definition
app :: App ViewState e Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const (attrMap V.defAttr [])
  }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: cui-log-viewer <filepath>"
    (filepath:_) -> runViewer filepath

runViewer :: FilePath -> IO ()
runViewer filepath = do
  -- Open LineCache
  cache <- openLineCache filepath
  
  -- Get file size using System.Directory
  fileSize <- getFileSize filepath
  
  -- Read first 25 lines with offsets
  initialLinesWithOffsets <- getLines cache 0 25
  
  -- Create initial cursor and viewport
  let cursor = ViewCursor { cursorOffset = 0, cursorLineNum = 0, cursorOrigin = FromStart }
      -- Calculate line numbers using the pure function
      linesWithNumbers = [(calculateDisplayLineNumber cursor i, line) 
                         | (i, line) <- zip [0..] initialLinesWithOffsets]
      initialState = ViewState
        { vsCache = cache
        , vsCursor = cursor
        , vsViewport = linesWithNumbers
        , vsViewportSize = 25
        , vsFilePath = filepath
        , vsFileSize = fileSize
        }
  
  -- Run brick app
  _ <- defaultMain app initialState
  
  -- Clean up
  closeLineCache cache
