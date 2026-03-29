{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Main (App(..), defaultMain, halt)
import Brick.Types (BrickEvent(..), EventM)
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

-- | Expand tab characters to spaces aligned to tab stops
expandTabs :: Int -> T.Text -> T.Text
expandTabs tabWidth = T.pack . go 0 . T.unpack
  where
    go _ [] = []
    go col ('\t':cs) =
      let spaces = tabWidth - (col `mod` tabWidth)
      in replicate spaces ' ' ++ go (col + spaces) cs
    go col (c:cs) = c : go (col + 1) cs

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
      let expanded = expandTabs (vsTabStop vs) text
          scrolled = T.drop (vsHScrollOffset vs) expanded
          lineNumStr = let s = show lineNum in replicate (6 - length s) ' ' ++ s
      in hBox [ str lineNumStr
              , str " "
              , txt scrolled
              ]
    
    -- Calculate position indicator
    cursor = vsCursor vs
    positionInfo = ""  -- Will be handled by line numbers
    
    -- Calculate line info using cursor line number bounds
    lineInfo = if cursorOrigin cursor == FromStart
               then "Lines: " ++ show (cursorFirstLine cursor) ++ "-" ++ show (cursorLastLine cursor)
               else "Lines: " ++ show (cursorFirstLine cursor) ++ " to " ++ show (cursorLastLine cursor)
    
    -- Status bar
    errorInfo = case vsError vs of
      Nothing  -> str ""
      Just msg -> str ("  ERROR: " ++ msg)

    statusBar = hBox
      [ str "File: "
      , str (vsFilePath vs)
      , str "  |  "
      , str lineInfo
      , errorInfo
      , str "  |  "
      , str "q:quit g:top G:end ↑↓:scroll ←→:pan PgUp/Dn:page 0:col0"
      ]
    
    -- Full viewport
    -- padBottom Max makes content greedy: Brick allocates remaining space
    -- after fixed-height borders and status bar (3 lines total).
    -- vsViewportSize drives cache loading; Brick handles display sizing.
    viewport = vBox
      [ hBorderWithLabel (str " CUI Log Viewer ")
      , padBottom Max $ vBox lineWidgets
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

-- Horizontal scroll (← → or h l)
handleEvent (VtyEvent (V.EvKey V.KRight [])) = do
  vs <- get
  put vs { vsHScrollOffset = vsHScrollOffset vs + 1 }
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = do
  vs <- get
  put vs { vsHScrollOffset = vsHScrollOffset vs + 1 }
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = do
  vs <- get
  put vs { vsHScrollOffset = max 0 (vsHScrollOffset vs - 1) }
handleEvent (VtyEvent (V.EvKey (V.KChar 'h') [])) = do
  vs <- get
  put vs { vsHScrollOffset = max 0 (vsHScrollOffset vs - 1) }
handleEvent (VtyEvent (V.EvKey (V.KChar '0') [])) = do
  vs <- get
  put vs { vsHScrollOffset = 0 }

-- Handle terminal resize
handleEvent (VtyEvent (V.EvResize width height)) = do
  vs <- get
  let uiChrome = 3  -- Top border + bottom border + status bar
      newViewportSize = max 5 (height - uiChrome)
  when (vsViewportSize vs /= newViewportSize) $ do
    vs' <- liftIO $ Ops.resizeViewport vs newViewportSize
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
