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
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.IO
import Data.Word
import Data.IORef
import Control.Exception
import Control.Monad (when)
 

type Offset = Integer

data LineMap = LineMap
  { lmPath      :: FilePath
  , lmHandle    :: Handle
  , lmIndexStep :: Int
  , lmForward   :: IORef (VS.Vector Word64) -- byte offsets at lines 0, K, 2K, ...
  , lmIndexed   :: IORef Integer -- how many lines have been scanned from start
  }

indexStepDefault :: Int
indexStepDefault = 1024

openLineMap :: FilePath -> Int -> IO LineMap
openLineMap path k = do
  h <- openBinaryFile path ReadMode
  fref <- newIORef (VS.singleton 0) -- offset 0 at line 0
  iref <- newIORef 0
  return $ LineMap path h k fref iref

closeLineMap :: LineMap -> IO ()
closeLineMap lm = hClose (lmHandle lm)

-- | Read up to 'count' lines starting at 'startLine' (startLine can be negative)
getLines :: LineMap -> Integer -> Int -> IO [T.Text]
getLines lm start count = do
  -- Translate negative start to positive by computing total lines (simple scan)
  startPos <- if start >= 0
    then return start
    else do
      total <- countTotalLines lm
      return $ max 0 (total + start)
  -- find nearest indexed line <= startPos
  let k = fromIntegral (lmIndexStep lm)
  let baseLine = (startPos `div` k) * k
  baseOffset <- findOrScanTo lm baseLine
  -- now scan forward from baseOffset until we reach startPos + count
  lns <- scanLinesFromOffset lm baseOffset baseLine startPos count
  return lns

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

-- Ensure index contains baseLine and return its offset, building index as we go
findOrScanTo :: LineMap -> Integer -> IO Offset
findOrScanTo lm baseLine = do
  if baseLine == 0 then return 0
  else do
    fwd <- readIORef (lmForward lm)
    let k = fromIntegral (lmIndexStep lm)
        targetIdx = fromIntegral (baseLine `div` k)
        lastIdx = VS.length fwd - 1
    
    if targetIdx <= lastIdx
      then return $ fromIntegral (fwd VS.! targetIdx)
      else do
        -- Need to scan forward from last known index to build up to targetIdx
        let startLine = fromIntegral lastIdx * k
            startOffset = fromIntegral (fwd VS.! lastIdx)
        scanAndBuildIndex lm startOffset startLine baseLine

-- Scan forward and build index entries at each indexStep interval
scanAndBuildIndex :: LineMap -> Offset -> Integer -> Integer -> IO Offset
scanAndBuildIndex lm startOffset startLine targetLine = do
  hSeek (lmHandle lm) AbsoluteSeek startOffset
  let k = fromIntegral (lmIndexStep lm)
      buf = 65536
      loop offset curLine = do
        if curLine >= targetLine
          then return offset
          else do
            bs <- BS.hGet (lmHandle lm) buf
            if BS.null bs
              then return offset
              else do
                let newlines = fromIntegral $ BS.count 10 bs
                    newOffset = offset + fromIntegral (BS.length bs)
                -- Check if we crossed any index boundaries and record them
                let oldIdx = curLine `div` k
                    newLine = curLine + newlines
                    newIdx = newLine `div` k
                when (newIdx > oldIdx) $ do
                  -- We crossed index boundaries, need to find exact offsets
                  recordIndexCrossings lm offset curLine bs k (oldIdx + 1) newIdx
                loop newOffset newLine
  loop startOffset startLine

-- Record byte offsets when crossing index boundaries while scanning
recordIndexCrossings :: LineMap -> Offset -> Integer -> BS.ByteString -> Integer -> Integer -> Integer -> IO ()
recordIndexCrossings lm baseOffset baseLine chunk k fromIdx toIdx = do
  when (fromIdx <= toIdx) $ do
    let targetLine = fromIdx * k
    -- Scan through chunk to find exact offset for targetLine
    offset <- findLineOffset baseOffset baseLine chunk targetLine
    -- Append to forward index
    modifyIORef' (lmForward lm) (\v -> VS.snoc v (fromIntegral offset))
    modifyIORef' (lmIndexed lm) (max targetLine)
    -- Recursively record next index
    recordIndexCrossings lm baseOffset baseLine chunk k (fromIdx + 1) toIdx

-- Find byte offset of a specific line within a chunk
findLineOffset :: Offset -> Integer -> BS.ByteString -> Integer -> IO Offset
findLineOffset baseOffset baseLine chunk targetLine = do
  let bytes = BS.unpack chunk
      findOff off _ [] = return off
      findOff off line (b:bs)
        | line >= targetLine = return off
        | b == 10 = findOff (off + 1) (line + 1) bs
        | otherwise = findOff (off + 1) line bs
  findOff baseOffset baseLine bytes

-- Scan lines from a byte offset and return requested slice as Text list
scanLinesFromOffset :: LineMap -> Offset -> Integer -> Integer -> Int -> IO [T.Text]
scanLinesFromOffset lm off baseLine startLine count = do
  let h = lmHandle lm
  hSeek h AbsoluteSeek off
  let go curLine acc remaining partial = do
        if remaining <= 0 then return acc
        else do
          bs <- BS.hGet h 65536
          if BS.null bs
            then do -- EOF: include partial only if it falls within requested window
              let includePartial = (not (BS.null partial)) && (curLine >= startLine) && (remaining > 0)
                  acc' = if includePartial then acc ++ [decodeUtf8Lenient partial] else acc
              return acc'
            else do
              let pieces = BS.split 10 bs -- split on LF
                  chunkEnded = (not (BS.null bs)) && (BS.last bs == 10)
              -- build list of complete pieces, and carry a new partial if chunk didn't end with NL
              let (textsBS, newPartial) = case pieces of
                    [] -> ([], partial)
                    [only] -> if chunkEnded
                                then ([if BS.null partial then only else BS.concat [partial, only]], BS.empty)
                                else ([], if BS.null partial then only else BS.concat [partial, only])
                    (p:ps) -> if chunkEnded
                                then ( (if BS.null partial then p else BS.concat [partial,p]) : ps , BS.empty)
                                else let mid = init ps
                                         lastp = last ps
                                         headCombined = if BS.null partial then p else BS.concat [partial,p]
                                     in ( headCombined : mid, lastp )
                  texts = map decodeUtf8Lenient textsBS
                  numNew = length texts
              -- collect requested slice
              let allLines = texts
                  needed = remaining
                  toTake = take needed (drop (fromIntegral (startLine - curLine)) allLines)
                  acc' = acc ++ map normalizeLine toTake
              if length toTake >= needed
                then return acc'
                else do
                  let newCur = curLine + fromIntegral numNew
                      newRemaining = remaining - length toTake
                  go newCur acc' newRemaining newPartial
  go baseLine [] count BS.empty

-- helpers
normalizeLine :: T.Text -> T.Text
normalizeLine = T.dropWhileEnd (== '\r')

-- UTF8 helpers
-- Local UTF-8 decode wrapper (lenient)
decodeUtf8Lenient :: BS.ByteString -> T.Text
decodeUtf8Lenient = TE.decodeUtf8With TEE.lenientDecode
