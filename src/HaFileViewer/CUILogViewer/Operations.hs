{-# LANGUAGE OverloadedStrings #-}

-- | CUILogViewer operations module
-- Exports all scroll/jump operations for use in both Main and tests
module HaFileViewer.CUILogViewer.Operations
  ( scrollDown
  , scrollUp
  , pageDown
  , pageUp
  , jumpToStart
  , jumpToEnd
  , initializeViewer
  , resizeViewport
  ) where

import HaFileViewer.CUILogViewer.ViewState
import HaFileViewer.Backend.LineCache
import HaFileViewer.Backend.BidirectionalScanner (Direction(..))

-- | Initialize viewer state from a file
initializeViewer :: FilePath -> Int -> IO ViewState
initializeViewer filepath viewportSize = do
  cache <- openLineCache filepath
  result <- getLinesFromStart cache viewportSize
  case result of
    LinesLoaded initialLines topPos bottomPos -> do
      let cursor = ViewCursor
            { cursorTopPosition    = topPos
            , cursorBottomPosition = bottomPos
            , cursorFirstLine      = fst (head initialLines)
            , cursorLastLine       = fst (last initialLines)
            , cursorOrigin         = lpOrigin topPos
            }
          initialState = ViewState
            { vsCache        = cache
            , vsCursor       = cursor
            , vsViewport     = take viewportSize initialLines
            , vsViewportSize = viewportSize
            , vsFilePath     = filepath
            , vsError        = Nothing
            }
      return initialState
    AtBoundary -> error "Cannot initialize viewer with empty file"
    LoadFailed msg -> error ("Cannot initialize viewer: " ++ msg)

-- | Scroll down by one line
scrollDown :: ViewState -> IO ViewState
scrollDown vs@ViewState{ vsViewport = [] } = return vs
scrollDown vs@ViewState
    { vsCursor = ViewCursor{ cursorOrigin       = origin
                            , cursorLastLine     = lastLine
                            , cursorBottomPosition = botPos }
    , vsCache  = cache }
  | origin == FromEnd && lastLine == -1 = return vs
  | otherwise =
      applyScrollDown vs <$>
        getLinesFrom cache botPos Forward 1 (lastLine + 1)

-- | Scroll up by one line
scrollUp :: ViewState -> IO ViewState
scrollUp vs@ViewState{ vsViewport = [] } = return vs
scrollUp vs@ViewState
    { vsCursor   = ViewCursor{ cursorOrigin    = origin
                              , cursorFirstLine = firstLine
                              , cursorTopPosition = topPos }
    , vsViewport = (firstLineNum, _) : _
    , vsCache    = cache }
  | origin == FromStart && firstLineNum <= 1 = return vs
  | otherwise =
      applyScrollUp vs <$>
        getLinesFrom cache topPos Backward 1 (firstLine - 1)

-- | Page down (scroll forward by viewport size)
pageDown :: ViewState -> IO ViewState
pageDown vs@ViewState{ vsViewport = [] } = return vs
pageDown vs@ViewState
    { vsCursor       = ViewCursor{ cursorBottomPosition = botPos
                                 , cursorLastLine       = lastLine }
    , vsCache        = cache
    , vsViewportSize = size } =
  applyLoad Nothing vs <$>
    getLinesFrom cache botPos Forward size (lastLine + 1)

-- | Page up (scroll backward by viewport size)
pageUp :: ViewState -> IO ViewState
pageUp vs@ViewState{ vsViewport = [] } = return vs
pageUp vs@ViewState
    { vsCursor       = ViewCursor{ cursorOrigin    = origin
                                 , cursorFirstLine = firstLine
                                 , cursorTopPosition = topPos }
    , vsViewport     = (firstLineNum, _) : _
    , vsCache        = cache
    , vsViewportSize = size }
  | origin == FromStart && firstLineNum <= 1 = return vs
  | otherwise =
      applyLoad Nothing vs <$>
        getLinesFrom cache topPos Backward size (firstLine - 1)

-- | Jump to start of file
jumpToStart :: ViewState -> IO ViewState
jumpToStart vs@ViewState{ vsCache = cache, vsViewportSize = size } =
  applyLoad (Just FromStart) vs <$> getLinesFromStart cache size

-- | Jump to end of file
jumpToEnd :: ViewState -> IO ViewState
jumpToEnd vs@ViewState{ vsCache = cache, vsViewportSize = size } =
  applyLoad (Just FromEnd) vs <$> getLinesFromEnd cache size

-- | Resize viewport to new height, preserving current scroll position
resizeViewport :: ViewState -> Int -> IO ViewState
resizeViewport vs@ViewState
    { vsCursor       = ViewCursor{ cursorOrigin    = origin
                                 , cursorTopPosition = topPos
                                 , cursorFirstLine   = firstLine }
    , vsCache        = cache
    , vsViewportSize = oldSize } newSize
  | newSize == oldSize = return vs
  | otherwise =
      applyLoad Nothing vs { vsViewportSize = newSize } <$>
        getLinesFrom cache topPos scrollDirection newSize firstLine
  where
    scrollDirection = case origin of
      FromStart -> Forward
      FromEnd   -> Backward

