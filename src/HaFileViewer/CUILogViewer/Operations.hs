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
  (initialLines, topPos, bottomPos) <- getLinesFromStart cache viewportSize
  if null initialLines
    then error "Cannot initialize viewer with empty file"
    else do
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
            }
      return initialState

-- | Scroll down by one line
scrollDown :: ViewState -> IO ViewState
scrollDown vs = do
  let cursor = vsCursor vs
      viewport = vsViewport vs
  if null viewport || (cursorOrigin cursor == FromEnd && cursorLastLine cursor == -1)
    then return vs
    else do
      (moreLines, topPos, bottomPos) <- getLinesFrom (vsCache vs)
                                          (cursorBottomPosition cursor)
                                          Forward 1
                                          (cursorLastLine cursor + 1)
      return $ applyScrollDown moreLines topPos bottomPos vs

-- | Scroll up by one line
scrollUp :: ViewState -> IO ViewState
scrollUp vs = do
  let cursor = vsCursor vs
      viewport = vsViewport vs
  if null viewport
    then return vs
    else do
      let (firstLineNum, _) = head viewport
      if cursorOrigin cursor == FromStart && firstLineNum <= 1
        then return vs
        else do
          (prevLines, topPos, bottomPos) <- getLinesFrom (vsCache vs)
                                              (cursorTopPosition cursor)
                                              Backward 1
                                              (cursorFirstLine cursor - 1)
          return $ applyScrollUp prevLines topPos bottomPos vs

-- | Page down (scroll forward by viewport size)
pageDown :: ViewState -> IO ViewState
pageDown vs = do
  let cursor = vsCursor vs
  if null (vsViewport vs)
    then return vs
    else do
      (nextPage, topPos, bottomPos) <- getLinesFrom (vsCache vs)
                                         (cursorBottomPosition cursor)
                                         Forward (vsViewportSize vs)
                                         (cursorLastLine cursor + 1)
      return $ applyLoad Nothing nextPage topPos bottomPos vs

-- | Page up (scroll backward by viewport size)
pageUp :: ViewState -> IO ViewState
pageUp vs = do
  let cursor = vsCursor vs
      viewport = vsViewport vs
  if null viewport
    then return vs
    else do
      let (firstLineNum, _) = head viewport
      if cursorOrigin cursor == FromStart && firstLineNum <= 1
        then return vs
        else do
          (prevPage, topPos, bottomPos) <- getLinesFrom (vsCache vs)
                                             (cursorTopPosition cursor)
                                             Backward (vsViewportSize vs)
                                             (cursorFirstLine cursor - 1)
          return $ applyLoad Nothing prevPage topPos bottomPos vs

-- | Jump to start of file
jumpToStart :: ViewState -> IO ViewState
jumpToStart vs = do
  (lines, topPos, bottomPos) <- getLinesFromStart (vsCache vs) (vsViewportSize vs)
  return $ applyLoad (Just (lpOrigin topPos)) lines topPos bottomPos vs

-- | Jump to end of file
jumpToEnd :: ViewState -> IO ViewState
jumpToEnd vs = do
  (lines, topPos, bottomPos) <- getLinesFromEnd (vsCache vs) (vsViewportSize vs)
  return $ applyLoad (Just (lpOrigin topPos)) lines topPos bottomPos vs

-- | Resize viewport to new height, preserving current scroll position
resizeViewport :: ViewState -> Int -> IO ViewState
resizeViewport vs newSize = do
  let cursor = vsCursor vs
  if newSize == vsViewportSize vs
    then return vs
    else do
      let scrollDirection = case cursorOrigin cursor of
            FromStart -> Forward
            FromEnd   -> Backward
      (newLines, topPos, bottomPos) <- getLinesFrom (vsCache vs)
                                         (cursorTopPosition cursor)
                                         scrollDirection newSize
                                         (cursorFirstLine cursor)
      return $ applyLoad Nothing newLines topPos bottomPos
                 vs { vsViewportSize = newSize }

