{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Main (App(..), defaultMain, halt)
import Brick.Types (BrickEvent(..), EventM)
import qualified Graphics.Vty as V
import qualified Data.Text as T
import HaFileViewer.CUILogViewer.ViewState
import HaFileViewer.LineCache
import System.Environment (getArgs)

-- Name type for brick
data Name = ViewportName deriving (Ord, Show, Eq)

-- Draw the UI
drawUI :: ViewState -> [Widget Name]
drawUI _ = [str "CUILogViewer - Loading... (press 'q' to quit)"]

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
  -- For now, just run with placeholder state
  -- We'll initialize properly in Step 7
  cache <- openLineCache filepath
  
  let placeholder = ViewState
        { vsCache = cache
        , vsCursor = ViewCursor 0 0 FromStart
        , vsViewport = []
        , vsViewportSize = 25
        , vsFilePath = filepath
        , vsFileSize = 0
        }
  
  _ <- defaultMain app placeholder
  
  closeLineCache cache
  return ()
