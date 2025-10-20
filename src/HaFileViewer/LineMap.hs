{-# LANGUAGE OverloadedStrings #-}
-- Minimal in-memory sparse line map for HaFileViewer.
module HaFileViewer.LineMap
  ( LineMap
  , openLineMap
  , closeLineMap
  , getLines
  , indexStepDefault
  ) where

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.IO
import Data.Int
import Data.Word
import Data.IORef
import Control.Monad
import Control.Exception
import Data.Maybe (fromMaybe)

type Offset = Integer

data LineMap = LineMap
  { lmPath      :: FilePath
  , lmHandle    :: Handle
  , lmIndexStep :: Int
  , lmForward   :: IORef (VS.Vector Word64) -- offsets at 0, K, 2K, ...
  , lmIndexed   :: IORef Integer -- how many lines have been scanned from start
  }

indexStepDefault :: Int
indexStepDefault = 1024

openLineMap :: FilePath -> Int -> IO LineMap
openLineMap path k = do
  h <- openBinaryFile path ReadMode
  fref <- newIORef VS.empty
  iref <- newIORef 0
  return $ LineMap path h k fref iref

closeLineMap :: LineMap -> IO ()
closeLineMap lm = hClose (lmHandle lm)

-- | Read up to 'count' lines starting at 'startLine' (startLine can be negative)
getLines :: LineMap -> Integer -> Int -> IO [T.Text]
getLines lm start count = do
  sz <- hFileSize (lmHandle lm)
  let fileBytes = fromIntegral sz :: Integer
  -- Translate negative start to positive by computing total lines (simple scan)
  startPos <- if start >= 0
    then return start
    else do
      total <- countTotalLines lm
      return $ max 0 (total + start)
  -- find nearest sparse index (naive: use indexedLines or 0)
  indexed <- readIORef (lmIndexed lm)
  let k = fromIntegral (lmIndexStep lm)
  let baseLine = (startPos `div` k) * k
  baseOffset <- if baseLine == 0 then return 0 else findOrScanTo lm baseLine
  -- now scan forward from baseOffset until we reach startPos + count
  lines <- scanLinesFromOffset lm baseOffset startPos count
  return lines

-- Count total lines (naive full scan) -- used only when negative indexing requested
countTotalLines :: LineMap -> IO Integer
countTotalLines lm = do
  h <- openBinaryFile (lmPath lm) ReadMode
  finally (go h 0) (hClose h)
  where
    go h acc = do
      bs <- BS.hGet h 65536
      if BS.null bs then return acc
      else do
        let n = fromIntegral $ BS.count 10 bs -- '
        go h (acc + n)

-- Ensure index contains baseLine and return its offset. This is a simplified version
findOrScanTo :: LineMap -> Integer -> IO Offset
findOrScanTo lm baseLine = do
  -- naive: scan from start until baseLine
  hSeek (lmHandle lm) AbsoluteSeek 0
  let buf = 65536
  let loop offset line = do
        bs <- BS.hGet (lmHandle lm) buf
        if BS.null bs then return offset
        else do
          let cnt = fromIntegral $ BS.count 10 bs
              linesNeeded = baseLine - line
          if linesNeeded <= 0
            then return offset
            else loop (offset + fromIntegral (BS.length bs)) (line + cnt)
  loop 0 0

-- Scan lines from a byte offset and return requested slice as Text list
scanLinesFromOffset :: LineMap -> Offset -> Integer -> Int -> IO [T.Text]
scanLinesFromOffset lm off startLine count = do
  let h = lmHandle lm
  hSeek h AbsoluteSeek off
  let go curLine acc remaining partial = do
        if remaining <= 0 then return (reverse acc)
        else do
          bs <- BS.hGet h 65536
          if BS.null bs
            then do -- EOF: if partial non-empty, include it
              let acc' = if BS.null partial then acc else (decodeUtf8Lenient partial):acc
              return (reverse acc')
            else do
              let pieces = BS.split 10 bs -- split on LF
                  withFirst = case partial of
                    bs0 | BS.null bs0 -> map decodeUtf8Lenient pieces
                        | otherwise -> let (p:ps) = pieces in decodeUtf8Lenient (BS.concat [bs0, p]) : map decodeUtf8Lenient ps
                  -- However we must convert pieces to Text properly and then slice
                  texts = map decodeUtf8Lenient pieces
                  numNew = length texts
                  startPosReached = curLine + fromIntegral numNew > fromIntegral startLine
              -- naive approach: collect lines until we have enough
              let allLines = texts
                  needed = remaining
                  -- skip lines before startLine
                  toTake = take needed (drop (fromIntegral (startLine - curLine)) allLines)
                  acc' = map normalizeLine toTake ++ acc
              if length toTake >= needed
                then return (reverse acc')
                else do
                  let newCur = curLine + fromIntegral numNew
                      newRemaining = remaining - length toTake
                  go newCur acc' newRemaining BS.empty
  go 0 [] count BS.empty

-- helpers
normalizeLine :: T.Text -> T.Text
normalizeLine = T.dropWhileEnd (== '\r')

-- UTF8 helpers
-- Local UTF-8 decode wrapper (lenient)
decodeUtf8Lenient :: BS.ByteString -> T.Text
decodeUtf8Lenient = TE.decodeUtf8With TEE.lenientDecode
