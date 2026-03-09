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
  (initialLines, topPos, bottomPos) <- getLinesFromStart cache viewportSize
  
  if null initialLines
    then error "Cannot initialize viewer with empty file"
    else do
      -- Swap tuple order: API returns (Text, Integer) but we need (Integer, Text)
      let swappedLines = [(lineNum, text) | (text, lineNum) <- initialLines]
      
      -- Calculate line number bounds
      let firstLineNum = 1
          lastLineNum = fromIntegral (length initialLines)
      
      -- Create initial cursor with two positions
      let cursor = ViewCursor 
            { cursorTopPosition = topPos
            , cursorBottomPosition = bottomPos
            , cursorFirstLine = firstLineNum
            , cursorLastLine = lastLineNum
            , cursorOrigin = lpOrigin topPos
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
      -- Use bottom position to read 1 more line forward
      -- startLineNum is the next line after current viewport
      let startLineNum = cursorLastLine cursor + 1
      (moreLines, topPos, bottomPos) <- getLinesFrom cache 
                                          (cursorBottomPosition cursor) 
                                          Forward 
                                          1 
                                          startLineNum
      
      if null moreLines
        then return vs  -- At EOF, don't change state
        else do
          -- Get the line with its number (swap tuple order: API returns (Text, Integer))
          let (text, lineNum) = head moreLines
              newLine = (lineNum, text)
          
          -- Shift viewport down
          let newViewport = shiftViewportDown viewport newLine (vsViewportSize vs)
          
          -- Update cursor with new positions and line numbers
          let newCursor = cursor 
                { cursorTopPosition = topPos
                , cursorBottomPosition = bottomPos
                , cursorFirstLine = cursorFirstLine cursor + 1
                , cursorLastLine = cursorLastLine cursor + 1
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
      -- Only block at start (FromStart origin), allow scrolling from end (negative lines)
      if cursorOrigin cursor == FromStart && firstLineNum <= 1
        then return vs
        else do
          -- Use top position to read 1 line backward
          -- startLineNum is the previous line before current viewport
          let startLineNum = cursorFirstLine cursor - 1
          (prevLines, topPos, bottomPos) <- getLinesFrom cache 
                                              (cursorTopPosition cursor) 
                                              Backward 
                                              1 
                                              startLineNum
          
          if null prevLines
            then return vs  -- Shouldn't happen, but be safe
            else do
              -- Get the line with its number (swap tuple order: API returns (Text, Integer))
              let (text, lineNum) = head prevLines
                  newLine = (lineNum, text)
              
              -- Shift viewport up
              let newViewport = shiftViewportUp newLine viewport (vsViewportSize vs)
              
              -- Update cursor with new positions and line numbers
              let newCursor = cursor
                    { cursorTopPosition = topPos
                    , cursorBottomPosition = bottomPos
                    , cursorFirstLine = cursorFirstLine cursor - 1
                    , cursorLastLine = cursorLastLine cursor - 1
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
      -- Read a full page forward from bottom position
      -- startLineNum is the next line after current viewport
      let startLineNum = cursorLastLine cursor + 1
      (nextPage, topPos, bottomPos) <- getLinesFrom cache 
                                        (cursorBottomPosition cursor) 
                                        Forward 
                                        pageSize 
                                        startLineNum
      
      if null nextPage
        then return vs  -- At EOF
        else do
          -- Swap tuple order: API returns (Text, Integer) but we need (Integer, Text)
          let swappedPage = [(lineNum, text) | (text, lineNum) <- nextPage]
              newFirstLine = cursorLastLine cursor + 1
              newLastLine = cursorLastLine cursor + fromIntegral (length nextPage)
          
          -- Update cursor and viewport
          let newCursor = cursor 
                { cursorTopPosition = topPos
                , cursorBottomPosition = bottomPos
                , cursorFirstLine = newFirstLine
                , cursorLastLine = newLastLine
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
      
      -- Can't page up if we're at beginning (line 1 with FromStart)
      -- Only block at start (FromStart origin), allow paging from end (negative lines)
      if cursorOrigin cursor == FromStart && firstLineNum <= 1
        then return vs
        else do
          -- Read a full page backward from top position
          -- startLineNum is the previous line before current viewport
          let startLineNum = cursorFirstLine cursor - 1
          (prevPage, topPos, bottomPos) <- getLinesFrom cache 
                                            (cursorTopPosition cursor) 
                                            Backward 
                                            pageSize 
                                            startLineNum
          
          if null prevPage
            then return vs
            else do
              -- Swap tuple order
              let swappedPage = [(lineNum, text) | (text, lineNum) <- prevPage]
                  newFirstLine = cursorFirstLine cursor - fromIntegral (length prevPage)
                  newLastLine = cursorFirstLine cursor - 1
              
              -- Update cursor and viewport
              let newCursor = cursor
                    { cursorTopPosition = topPos
                    , cursorBottomPosition = bottomPos
                    , cursorFirstLine = newFirstLine
                    , cursorLastLine = newLastLine
                    }
              
              return vs { vsViewport = swappedPage, vsCursor = newCursor }

-- | Jump to start of file
jumpToStart :: ViewState -> IO ViewState
jumpToStart vs = do
  let cache = vsCache vs
      pageSize = vsViewportSize vs
  
  -- Use new API: get lines from start (returns 3 values)
  (linesWithNumbers, topPos, bottomPos) <- getLinesFromStart cache pageSize
  
  if null linesWithNumbers
    then return vs  -- Empty file
    else do
      -- Swap tuple order: API returns (Text, Integer) but we need (Integer, Text)
      let swappedLines = [(lineNum, text) | (text, lineNum) <- linesWithNumbers]
          firstLineNum = 1
          lastLineNum = fromIntegral (length linesWithNumbers)
      
      -- Create cursor at file start with two positions
      let newCursor = ViewCursor
            { cursorTopPosition = topPos
            , cursorBottomPosition = bottomPos
            , cursorFirstLine = firstLineNum
            , cursorLastLine = lastLineNum
            , cursorOrigin = lpOrigin topPos
            }
      
      return vs { vsViewport = swappedLines
                , vsCursor = newCursor
                }

-- | Jump to end of file
jumpToEnd :: ViewState -> IO ViewState
jumpToEnd vs = do
  let cache = vsCache vs
      pageSize = vsViewportSize vs
  
  -- Use new API: get lines from end (returns 3 values)
  (linesWithNumbers, topPos, bottomPos) <- getLinesFromEnd cache pageSize
  
  if null linesWithNumbers
    then return vs  -- Empty file
    else do
      -- Swap tuple order: API returns (Text, Integer) but we need (Integer, Text)
      let swappedLines = [(lineNum, text) | (text, lineNum) <- linesWithNumbers]
          firstLineNum = negate (fromIntegral (length linesWithNumbers))
          lastLineNum = -1
      
      -- Create cursor at file end with two positions
      let newCursor = ViewCursor
            { cursorTopPosition = topPos
            , cursorBottomPosition = bottomPos
            , cursorFirstLine = firstLineNum
            , cursorLastLine = lastLineNum
            , cursorOrigin = lpOrigin topPos
            }
      
      return vs { vsViewport = swappedLines
                , vsCursor = newCursor
                }
