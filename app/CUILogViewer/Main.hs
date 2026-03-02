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
import System.Directory (getFileSize, doesFileExist)
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
    positionInfo = if cursorOffset cursor == 0
                   then " [START] "
                   else if cursorOffset cursor >= vsFileSize vs
                   then " [END] "
                   else ""
    
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
  
  -- At EOF, don't scroll
  if null viewport
    then return vs
    else do
      -- Get the line number of the last line in viewport
      let (lastLineNum, _) = last viewport
          -- For FromStart origin, line numbers are positive (1-based)
          -- So the next line to read is at 0-based index: lastLineNum
          nextLineIndex = case cursorOrigin cursor of
            FromStart -> lastLineNum  -- lastLineNum is 1-based, convert to 0-based
            FromEnd -> error "Scrolling from end not yet implemented"
      
      -- Read 1 more line
      moreLines <- getLines cache nextLineIndex 1
      
      if null moreLines
        then return vs  -- At EOF, don't change state
        else do
          -- Create new line with number
          let newLine = (lastLineNum + 1, head moreLines)
          
          -- Shift viewport down
          let newViewport = shiftViewportDown viewport newLine (vsViewportSize vs)
          
          -- Update cursor line number
          let newCursor = cursor { cursorLineNum = cursorLineNum cursor + 1 }
          
          return vs { vsViewport = newViewport, vsCursor = newCursor }

scrollUp :: ViewState -> IO ViewState
scrollUp vs = do
  let cache = vsCache vs
      cursor = vsCursor vs
      viewport = vsViewport vs
  
  -- Can't scroll up from beginning
  if null viewport
    then return vs
    else do
      -- Get the line number of the first line in viewport
      let (firstLineNum, _) = head viewport
      
      -- Can't scroll up if we're at line 1 (beginning of file)
      if firstLineNum <= 1
        then return vs
        else do
          -- Read the previous line (0-based index is firstLineNum - 2)
          let prevLineIndex = firstLineNum - 2
          prevLines <- getLines cache prevLineIndex 1
          
          if null prevLines
            then return vs  -- Shouldn't happen, but be safe
            else do
              -- Create new line with number
              let newLine = (firstLineNum - 1, head prevLines)
              
              -- Shift viewport up
              let newViewport = shiftViewportUp newLine viewport (vsViewportSize vs)
              
              -- Update cursor line number
              let newCursor = cursor { cursorLineNum = cursorLineNum cursor - 1 }
              
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
      -- Get the line number of the last line in viewport
      let (lastLineNum, _) = last viewport
          -- Next page starts right after current viewport
          nextPageIndex = lastLineNum
      
      -- Read a full page of lines
      nextPage <- getLines cache nextPageIndex pageSize
      
      if null nextPage
        then return vs  -- At EOF
        else do
          -- Create new viewport with line numbers
          let newLineNumbers = [lastLineNum + 1 .. lastLineNum + fromIntegral (length nextPage)]
              newViewport = zip newLineNumbers nextPage
          
          -- Update cursor (advance by the number of lines read)
          let newCursor = cursor { cursorLineNum = cursorLineNum cursor + fromIntegral (length nextPage) }
          
          return vs { vsViewport = newViewport, vsCursor = newCursor }

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
          -- Calculate how many lines to go back (full page or less if near start)
          let linesToRead = min pageSize (fromIntegral firstLineNum - 1)
              prevPageIndex = max 0 (firstLineNum - fromIntegral linesToRead - 1)
          
          -- Read previous page
          prevPage <- getLines cache prevPageIndex linesToRead
          
          if null prevPage
            then return vs
            else do
              -- Create new viewport with line numbers
              let newLineNumbers = [prevPageIndex + 1 .. prevPageIndex + fromIntegral (length prevPage)]
                  newViewport = zip newLineNumbers prevPage
              
              -- Update cursor (go back by the number of lines read)
              let newCursor = cursor { cursorLineNum = cursorLineNum cursor - fromIntegral (length prevPage) }
              
              return vs { vsViewport = newViewport, vsCursor = newCursor }

-- Jump to start of file
jumpToStart :: ViewState -> IO ViewState
jumpToStart vs = do
  let cache = vsCache vs
      pageSize = vsViewportSize vs
  
  -- Read first page from offset 0
  firstPage <- getLines cache 0 pageSize
  
  if null firstPage
    then return vs  -- Empty file
    else do
      -- Create cursor at file start
      let newCursor = ViewCursor
            { cursorOffset = 0
            , cursorLineNum = 0
            , cursorOrigin = FromStart
            }
      
      -- Calculate line numbers starting from 1
      let linesWithNumbers = [(calculateDisplayLineNumber newCursor i, line) 
                             | (i, line) <- zip [0..] firstPage]
      
      return vs { vsViewport = linesWithNumbers
                , vsCursor = newCursor
                }

-- Jump to end of file
jumpToEnd :: ViewState -> IO ViewState
jumpToEnd vs = do
  let cache = vsCache vs
      pageSize = vsViewportSize vs
      fileSize = vsFileSize vs
  
  -- Approximate: read from near end to find last lines
  -- This is a simple approach - read from a position well before EOF
  let approxStart = max 0 (fromIntegral fileSize - fromIntegral (pageSize * 100))
  allLines <- getLines cache approxStart (pageSize * 2)
  
  if null allLines
    then return vs  -- Empty file
    else do
      -- Take last pageSize lines
      let lastPage = drop (max 0 (length allLines - pageSize)) allLines
      
      -- Create cursor at file end
      let newCursor = ViewCursor
            { cursorOffset = fromIntegral fileSize
            , cursorLineNum = 0
            , cursorOrigin = FromEnd
            }
      
      -- Calculate NEGATIVE line numbers (viewing from end)
      let linesWithNumbers = [(calculateDisplayLineNumber newCursor i, line) 
                             | (i, line) <- zip [0..] lastPage]
      
      return vs { vsViewport = linesWithNumbers
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
  
  -- Get file size using System.Directory
  fileSize <- getFileSize filepath
  
  -- Check for empty file
  if fileSize == 0
    then do
      putStrLn "Error: File is empty"
      closeLineCache cache
    else do
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
