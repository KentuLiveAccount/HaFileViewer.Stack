{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import HaFileViewer.Backend
import HaFileViewer.LineMap
import Data.IORef
import System.IO
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Aeson (object, (.=))
import Network.HTTP.Types.Status (status400)
import qualified Data.Text as T
import Data.List (isInfixOf)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = do
  cacheRef <- newIORef Nothing :: IO (IORef (Maybe (FilePath, LineMap)))
  scotty 3000 $ do
    middleware logStdoutDev
    get "/open" $ do
      setHeader "Access-Control-Allow-Origin" "*"
      ps <- params
      let mpath = lookup ("path" :: T.Text) ps
      case mpath of
        Nothing -> do
          status status400
          json (object ["error" .= ("missing path" :: String)])
        Just p -> do
          let path = T.unpack p
          when (".." `isInfixOf` path) $ do
            status status400
            json (object ["error" .= ("invalid path" :: String)])
          fh <- liftIO $ openFileHandle path
          json $ object ["size" .= (fileSize fh)]

    get "/range" $ do
      setHeader "Access-Control-Allow-Origin" "*"
      ps <- params
      case lookup ("path" :: T.Text) ps of
        Nothing -> do
          status status400
          json (object ["error" .= ("missing path" :: String)])
        Just p -> do
          let path = T.unpack p
          when (".." `isInfixOf` path) $ do
            status status400
            json (object ["error" .= ("invalid path" :: String)])
          let off = fromMaybe 0 (lookup "off" ps >>= (readMaybe . T.unpack)) :: Integer
              len = fromMaybe 4096 (lookup "len" ps >>= (readMaybe . T.unpack)) :: Int
          -- For /range we use the Backend file handle API to read bytes
          fh <- liftIO $ openFileHandle path
          bs <- liftIO $ readChunkAt fh off len
          liftIO $ closeFileHandle fh
          raw (BL.fromStrict bs)

    -- lines endpoint: start can be negative
    get "/lines" $ do
      setHeader "Access-Control-Allow-Origin" "*"
      ps <- params
      case lookup ("path" :: T.Text) ps of
        Nothing -> do status status400; json (object ["error" .= ("missing path" :: String)])
        Just p -> do
          let path = T.unpack p
          when (".." `isInfixOf` path) $ do
            status status400
            json (object ["error" .= ("invalid path" :: String)])
          let start = fromMaybe 0 (lookup "start" ps >>= (readMaybe . T.unpack)) :: Integer
              cnt = fromMaybe 100 (lookup "count" ps >>= (readMaybe . T.unpack)) :: Int
          mLm <- liftIO $ readIORef cacheRef
          lm <- case mLm of
            Just (p, existing) | p == path -> return existing
            _ -> do
              newLm <- liftIO (openLineMap path indexStepDefault)
              liftIO $ writeIORef cacheRef (Just (path, newLm))
              return newLm
          ls <- liftIO $ getLines lm start cnt
          json $ object ["start" .= start, "countRequested" .= cnt, "lines" .= ls]
