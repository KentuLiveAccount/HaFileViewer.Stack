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
import HaFileViewer.BidirectionalScanner (Direction(..))
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Control.Monad.IO.Class (liftIO)

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
    
    -- Calculate line info
    lineInfo = if cursorOrigin cursor == FromStart
               then "Lines: " ++ show (cursorLineNum cursor + 1) ++ "+..."
               else "Lines: ...-" ++ show (abs (cursorLineNum cursor))
    
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

-- Scroll down (↓ or j)
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
  vs <- get
  vs' <- liftIO (scrollDown vs)
  put vs'
handleEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = do
  vs <- get
  vs' <- liftIO (scrollDown vs)
  put vs'

-- Scroll up (↑ or k)
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
  vs <- get
  vs' <- liftIO (scrollUp vs)
  put vs'
handleEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = do
  vs <- get
  vs' <- liftIO (scrollUp vs)
  put vs'

-- Page down (PgDn)
handleEvent (VtyEvent (V.EvKey V.KPageDown [])) = do
  vs <- get
  vs' <- liftIO (pageDown vs)
  put vs'

-- Page up (PgUp)
handleEvent (VtyEvent (V.EvKey V.KPageUp [])) = do
  vs <- get
  vs' <- liftIO (pageUp vs)
  put vs'

-- Jump to start (Home or g)
handleEvent (VtyEvent (V.EvKey V.KHome [])) = do
  vs <- get
  vs' <- liftIO (jumpToStart vs)
  put vs'
handleEvent (VtyEvent (V.EvKey (V.KChar 'g') [])) = do
  vs <- get
  vs' <- liftIO (jumpToStart vs)
  put vs'

-- Jump to end (End or G)
handleEvent (VtyEvent (V.EvKey V.KEnd [])) = do
  vs <- get
  vs' <- liftIO (jumpToEnd vs)
  put vs'
handleEvent (VtyEvent (V.EvKey (V.KChar 'G') [])) = do
  vs <- get
  vs' <- liftIO (jumpToEnd vs)
  put vs'

handleEvent _ = return ()

-- Scrolling operations
scrollDown :: ViewState -> IO ViewState
scrollDown vs = do
  let cache = vsCache vs
      cursor = vsCursor vs
      viewport = vsViewport vs
  
  -- At EOF or empty viewport, don't scroll
  if null viewport
    then return vs
    else do
      -- Use the cursor position to read 1 more line forward
      (moreLines, newPosition) <- getLinesFrom cache (cursorPosition cursor) Forward 1
      
      if null moreLines
        then return vs  -- At EOF, don't change state
        else do
          -- Get the line with its number (swap tuple order: API returns (Text, Integer))
          let (text, lineNum) = head moreLines
              newLine = (lineNum, text)
          
          -- Shift viewport down
          let newViewport = shiftViewportDown viewport newLine (vsViewportSize vs)
          
          -- Update cursor
          let newCursor = cursor 
                { cursorPosition = newPosition
                , cursorLineNum = cursorLineNum cursor + 1
                }
          
          return vs { vsViewport = newViewport, vsCursor = newCursor }

scrollUp :: ViewState -> IO ViewState
scrollUp vs = do
  let cache = vsCache vs
      cursor = vsCursor vs
      viewport = vsViewport vs
  
  -- Can't scroll up from beginning or empty viewport
  if null viewport
    then return vs
    else do
      -- Get the line number of the first line in viewport
      let (firstLineNum, _) = head viewport
      
      -- Can't scroll up if we're at line 1 (beginning of file)
      if firstLineNum <= 1
        then return vs
        else do
          -- Use cursor position to read 1 line backward
          (prevLines, newPosition) <- getLinesFrom cache (cursorPosition cursor) Backward 1
          
          if null prevLines
            then return vs  -- Shouldn't happen, but be safe
            else do
              -- Get the line with its number (swap tuple order: API returns (Text, Integer))
              let (text, lineNum) = head prevLines
                  newLine = (lineNum, text)
              
              -- Shift viewport up
              let newViewport = shiftViewportUp newLine viewport (vsViewportSize vs)
              
              -- Update cursor
              let newCursor = cursor 
                    { cursorPosition = newPosition
                    , cursorLineNum = cursorLineNum cursor - 1
                    }
              
              return vs { vsViewport = newViewport, vsCursor = newCursor }

pageDown :: ViewState -> IO ViewState
pageDown vs = do
  let cache = vsCache vs
      cursor = vsCursor vs
      viewport = vsViewport vs
      pageSize = vsViewportSize vs
  
  if null viewport
    then return vs
    else do
      -- Read a full page forward from current position
      (nextPage, newPosition) <- getLinesFrom cache (cursorPosition cursor) Forward pageSize
      
      if null nextPage
        then return vs  -- At EOF
        else do
          -- Swap tuple order: API returns (Text, Integer) but we need (Integer, Text)
          let swappedPage = [(lineNum, text) | (text, lineNum) <- nextPage]
          
          -- Update cursor and viewport
          let newCursor = cursor 
                { cursorPosition = newPosition
                , cursorLineNum = cursorLineNum cursor + fromIntegral (length nextPage)
                }
          
          return vs { vsViewport = swappedPage, vsCursor = newCursor }

pageUp :: ViewState -> IO ViewState
pageUp vs = do
  let cache = vsCache vs
      cursor = vsCursor vs
      viewport = vsViewport vs
      pageSize = vsViewportSize vs
  
  if null viewport
    then return vs
    else do
      -- Get the line number of the first line in viewport
      let (firstLineNum, _) = head viewport
      
      -- Can't scroll up if we're at the beginning
      if firstLineNum <= 1
        then return vs
        else do
          -- Read a full page backward from current position
          (prevPage, newPosition) <- getLinesFrom cache (cursorPosition cursor) Backward pageSize
          
          if null prevPage
            then return vs
            else do
              -- Swap tuple order: API returns (Text, Integer) but we need (Integer, Text)
              let swappedPage = [(lineNum, text) | (text, lineNum) <- prevPage]
              
              -- Update cursor and viewport
              let newCursor = cursor 
                    { cursorPosition = newPosition
                    , cursorLineNum = cursorLineNum cursor - fromIntegral (length prevPage)
                    }
              
              return vs { vsViewport = swappedPage, vsCursor = newCursor }

-- Jump to start of file
jumpToStart :: ViewState -> IO ViewState
jumpToStart vs = do
  let cache = vsCache vs
      pageSize = vsViewportSize vs
  
  -- Use new API: get lines from start
  (linesWithNumbers, newPosition) <- getLinesFromStart cache pageSize
  
  if null linesWithNumbers
    then return vs  -- Empty file
    else do
      -- Swap tuple order: API returns (Text, Integer) but we need (Integer, Text)
      let swappedLines = [(lineNum, text) | (text, lineNum) <- linesWithNumbers]
      
      -- Create cursor at file start
      let newCursor = ViewCursor
            { cursorPosition = newPosition
            , cursorLineNum = fromIntegral (length linesWithNumbers)
            , cursorOrigin = FromStart
            }
      
      return vs { vsViewport = swappedLines
                , vsCursor = newCursor
                }

-- Jump to end of file
jumpToEnd :: ViewState -> IO ViewState
jumpToEnd vs = do
  let cache = vsCache vs
      pageSize = vsViewportSize vs
  
  -- Use new API: get lines from end
  (linesWithNumbers, newPosition) <- getLinesFromEnd cache pageSize
  
  if null linesWithNumbers
    then return vs  -- Empty file
    else do
      -- Swap tuple order: API returns (Text, Integer) but we need (Integer, Text)
      let swappedLines = [(lineNum, text) | (text, lineNum) <- linesWithNumbers]
      
      -- Create cursor at file end
      let newCursor = ViewCursor
            { cursorPosition = newPosition
            , cursorLineNum = fromIntegral (length linesWithNumbers)
            , cursorOrigin = FromEnd
            }
      
      return vs { vsViewport = swappedLines
                , vsCursor = newCursor
                }

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
  -- Open LineCache
  cache <- openLineCache filepath
  
  -- Read first 25 lines with line numbers using new API
  (initialLines, initialPosition) <- getLinesFromStart cache 25
  
  -- Check for empty file
  if null initialLines
    then do
      putStrLn "Error: File is empty"
      closeLineCache cache
    else do
      -- Swap tuple order: API returns (Text, Integer) but we need (Integer, Text)
      let swappedLines = [(lineNum, text) | (text, lineNum) <- initialLines]
      
      -- Create initial cursor and viewport
      let cursor = ViewCursor 
            { cursorPosition = initialPosition
            , cursorLineNum = fromIntegral (length initialLines)
            , cursorOrigin = FromStart
            }
          initialState = ViewState
            { vsCache = cache
            , vsCursor = cursor
            , vsViewport = swappedLines
            , vsViewportSize = 25
            , vsFilePath = filepath
            }
      
      -- Run brick app
      _ <- defaultMain app initialState
      
      -- Clean up
      closeLineCache cache
