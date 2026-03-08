{-# LANGUAGE OverloadedStrings #-}

-- | CUILogViewer operations module
-- Exports all scroll/jump operations for use in both Main and tests
module Operations
  ( scrollDown
  , scrollUp
  , pageDown
  , pageUp
  , jumpToStart
  , jumpToEnd
  , initializeViewer
  ) where

import qualified Data.Text as T
import HaFileViewer.CUILogViewer.ViewState
import HaFileViewer.LineCache
import HaFileViewer.BidirectionalScanner (Direction(..))

-- | Initialize viewer state from a file
initializeViewer :: FilePath -> Int -> IO ViewState
initializeViewer filepath viewportSize = do
  cache <- openLineCache filepath
  (initialLines, initialPosition) <- getLinesFromStart cache viewportSize
  
  if null initialLines
    then error "Cannot initialize viewer with empty file"
    else do
      -- Swap tuple order: API returns (Text, Integer) but we need (Integer, Text)
      let swappedLines = [(lineNum, text) | (text, lineNum) <- initialLines]
      
      -- Create initial cursor and viewport
      let cursor = ViewCursor 
            { cursorPosition = initialPosition
            , cursorOrigin = lpOrigin initialPosition
            }
          
          initialState = ViewState
            { vsCache = cache
            , vsCursor = cursor
            , vsViewport = swappedLines
            , vsViewportSize = viewportSize
            , vsFilePath = filepath
            }
      
      return initialState

-- | Scroll down by one line
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
                }
          
          return vs { vsViewport = newViewport, vsCursor = newCursor }

-- | Scroll up by one line
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
      -- BUG: Missing cursorOrigin check - blocks at end too!
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
                    }
              
              return vs { vsViewport = newViewport, vsCursor = newCursor }

-- | Page down (scroll forward by viewport size)
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
                }
          
          return vs { vsViewport = swappedPage, vsCursor = newCursor }

-- | Page up (scroll backward by viewport size)
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
      
      -- Can't page up if we're at line 1 or before
      -- BUG: Missing cursorOrigin check - blocks at end too!
      if firstLineNum <= 1
        then return vs
        else do
          -- Read a full page backward
          (prevPage, newPosition) <- getLinesFrom cache (cursorPosition cursor) Backward pageSize
          
          if null prevPage
            then return vs
            else do
              -- Swap tuple order
              let swappedPage = [(lineNum, text) | (text, lineNum) <- prevPage]
              
              -- Update cursor and viewport
              let newCursor = cursor
                    { cursorPosition = newPosition
                    }
              
              return vs { vsViewport = swappedPage, vsCursor = newCursor }

-- | Jump to start of file
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
            , cursorOrigin = lpOrigin newPosition
            }
      
      return vs { vsViewport = swappedLines
                , vsCursor = newCursor
                }

-- | Jump to end of file
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
            , cursorOrigin = lpOrigin newPosition
            }
      
      return vs { vsViewport = swappedLines
                , vsCursor = newCursor
                }
