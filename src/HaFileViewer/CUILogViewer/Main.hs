{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Main (App(..), defaultMain, halt)
import Brick.Types (BrickEvent(..), EventM, Context, availHeightL)
import Brick.Widgets.Border (hBorder, hBorderWithLabel)
import qualified Graphics.Vty as V
import qualified Data.Text as T
import HaFileViewer.CUILogViewer.ViewState
import HaFileViewer.Backend.LineCache
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified HaFileViewer.CUILogViewer.Operations as Ops
import Lens.Micro ((^.))

-- Name type for brick
data Name = ViewportName deriving (Ord, Show, Eq)

-- Draw the UI
drawUI :: ViewState -> [Widget Name]
drawUI vs = [viewport]
  where
    -- Render each line with line number
    lineWidgets = if null (vsViewport vs)
                  then [str "(empty file)"]
                  else map renderLine (vsViewport vs)
    renderLine (lineNum, text) = 
      hBox [ padLeft (Pad 1) $ str (show lineNum)
           , str ": "
           , txt text
           ]
    
    -- Calculate position indicator
    cursor = vsCursor vs
    positionInfo = ""  -- Will be handled by line numbers
    
    -- Calculate line info using cursor line number bounds
    lineInfo = if cursorOrigin cursor == FromStart
               then "Lines: " ++ show (cursorFirstLine cursor) ++ "-" ++ show (cursorLastLine cursor)
               else "Lines: " ++ show (cursorFirstLine cursor) ++ " to " ++ show (cursorLastLine cursor)
    
    -- Status bar
    statusBar = hBox
      [ str "File: "
      , str (vsFilePath vs)
      , str "  |  "
      , str lineInfo
      , str positionInfo
      , str "  |  "
      , str "q:quit g:top G:end ↑↓:scroll PgUp/Dn:page"
      ]
    
    -- Full viewport
    -- Limit the content area to exactly vsViewportSize to prevent overflow
    contentHeight = vsViewportSize vs
    viewport = vBox
      [ hBorderWithLabel (str " CUI Log Viewer ")
      , hLimit maxBound $ vLimit contentHeight $ vBox lineWidgets
      , hBorder
      , statusBar
      ]

-- Handle keyboard events
handleEvent :: BrickEvent Name e -> EventM Name ViewState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt

-- Scroll down (↓ or j)
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
  vs <- get
  vs' <- liftIO (Ops.scrollDown vs)
  put vs'
handleEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = do
  vs <- get
  vs' <- liftIO (Ops.scrollDown vs)
  put vs'

-- Scroll up (↑ or k)
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
  vs <- get
  vs' <- liftIO (Ops.scrollUp vs)
  put vs'
handleEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = do
  vs <- get
  vs' <- liftIO (Ops.scrollUp vs)
  put vs'

-- Page down (PgDn)
handleEvent (VtyEvent (V.EvKey V.KPageDown [])) = do
  vs <- get
  vs' <- liftIO (Ops.pageDown vs)
  put vs'

-- Page up (PgUp)
handleEvent (VtyEvent (V.EvKey V.KPageUp [])) = do
  vs <- get
  vs' <- liftIO (Ops.pageUp vs)
  put vs'

-- Jump to start (Home or g)
handleEvent (VtyEvent (V.EvKey V.KHome [])) = do
  vs <- get
  vs' <- liftIO (Ops.jumpToStart vs)
  put vs'
handleEvent (VtyEvent (V.EvKey (V.KChar 'g') [])) = do
  vs <- get
  vs' <- liftIO (Ops.jumpToStart vs)
  put vs'

-- Jump to end (End or G)
handleEvent (VtyEvent (V.EvKey V.KEnd [])) = do
  vs <- get
  vs' <- liftIO (Ops.jumpToEnd vs)
  put vs'
handleEvent (VtyEvent (V.EvKey (V.KChar 'G') [])) = do
  vs <- get
  vs' <- liftIO (Ops.jumpToEnd vs)
  put vs'

-- Handle terminal resize
handleEvent (VtyEvent (V.EvResize width height)) = do
  vs <- get
  let uiChrome = 3  -- Top border + bottom border + status bar
      newViewportSize = max 5 (height - uiChrome)
      oldSize = vsViewportSize vs
      logPath = "C:\\GitHub\\HaFileViewer.Stack\\resize_debug.log"
  
  -- Debug: Log resize event with absolute path
  liftIO $ appendFile logPath $ 
    "RESIZE: height=" ++ show height ++ 
    " oldSize=" ++ show oldSize ++ 
    " newSize=" ++ show newViewportSize ++
    " firstLine=" ++ show (cursorFirstLine (vsCursor vs)) ++
    " origin=" ++ show (cursorOrigin (vsCursor vs)) ++ "\n"
  
  -- Only resize if size actually changed
  when (vsViewportSize vs /= newViewportSize) $ do
    vs' <- liftIO $ Ops.resizeViewport vs newViewportSize
    liftIO $ appendFile logPath $
      "  AFTER: firstLine=" ++ show (cursorFirstLine (vsCursor vs')) ++
      " viewport lines=" ++ show (length (vsViewport vs')) ++ "\n"
    put vs'

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
    (filepath:_) -> do
      exists <- doesFileExist filepath
      if exists
        then runViewer filepath
        else putStrLn $ "Error: File not found: " ++ filepath

runViewer :: FilePath -> IO ()
runViewer filepath = do
  -- Start with a reasonable default (will be resized by Brick on startup)
  let initialViewportSize = 21  -- Reasonable default for 24-line terminal
  
  -- Initialize viewer using Operations module
  initialState <- Ops.initializeViewer filepath initialViewportSize
  
  -- Run brick app (appStartEvent will resize based on actual terminal)
  _ <- defaultMain app initialState
  
  -- Clean up
  closeLineCache (vsCache initialState)
