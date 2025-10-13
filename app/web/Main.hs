{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import HaFileViewer.Backend
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Aeson (object, (.=))
import Network.HTTP.Types.Status (status400)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  get "/open" $ do
    ps <- params
    let mpath = lookup ("path" :: T.Text) ps
    case mpath of
      Nothing -> do
        status status400
        json (object ["error" .= ("missing path" :: String)])
      Just p -> do
        let path = T.unpack p
        fh <- liftIO $ openFileHandle path
        json $ object ["size" .= (fileSize fh)]

  get "/range" $ do
    ps <- params
    case lookup ("path" :: T.Text) ps of
      Nothing -> do
        status status400
        json (object ["error" .= ("missing path" :: String)])
      Just p -> do
        let path = T.unpack p
            off = fromMaybe 0 (lookup "off" ps >>= (readMaybe . T.unpack)) :: Integer
            len = fromMaybe 4096 (lookup "len" ps >>= (readMaybe . T.unpack)) :: Int
        fh <- liftIO $ openFileHandle path
        bs <- liftIO $ readChunkAt fh off len
        liftIO $ closeFileHandle fh
        raw (BL.fromStrict bs)
