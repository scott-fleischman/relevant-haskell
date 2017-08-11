{-# LANGUAGE OverloadedStrings #-}

module Common where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text.Encoding
import qualified Data.Text.Lazy.IO    as Text.Lazy.IO
import qualified Network.HTTP.Client  as HTTP.Client
import qualified Network.HTTP.Simple  as HTTP.Simple
import qualified Text.Pretty.Simple   as Pretty.Simple

pPrintJSON :: Aeson.ToJSON a => a -> IO ()
pPrintJSON
  = Text.Lazy.IO.putStrLn
  . Pretty.Simple.pString
  . Text.unpack
  . Text.Encoding.decodeUtf8
  . ByteString.Lazy.toStrict
  . Aeson.encode
  . Aeson.toJSON

pPrintResponse :: Aeson.ToJSON a => HTTP.Client.Response a -> IO ()
pPrintResponse = pPrintJSON . HTTP.Client.responseBody

host :: Text.Text
host = "127.0.0.1"

port :: Int
port = 9200

baseRequest :: HTTP.Simple.Request
baseRequest
  = HTTP.Simple.setRequestMethod "GET"
  . HTTP.Simple.setRequestPort port
  . HTTP.Simple.setRequestHost (Text.Encoding.encodeUtf8 host)
  $ HTTP.Simple.defaultRequest
