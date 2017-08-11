{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import           Data.Semigroup       ((<>))
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text.Encoding
import qualified Data.Text.Lazy.IO    as Text.Lazy.IO
import qualified Network.HTTP.Client  as HTTP.Client
import qualified Network.HTTP.Simple  as HTTP.Simple
import qualified Text.Pretty.Simple   as Pretty.Simple

pPrintJSON :: Aeson.Value -> IO ()
pPrintJSON
  = Text.Lazy.IO.putStrLn
  . Pretty.Simple.pString
  . Text.unpack
  . Text.Encoding.decodeUtf8
  . ByteString.Lazy.toStrict
  . Aeson.encode

pPrintToJSON :: Aeson.ToJSON a => a -> IO ()
pPrintToJSON = pPrintJSON . Aeson.toJSON

pPrintResponse :: HTTP.Client.Response Aeson.Value -> IO ()
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

createIndex :: Aeson.ToJSON a => Text.Text -> a -> IO ()
createIndex name settings
  = sendRequest_
  . HTTP.Simple.setRequestBodyJSON settings
  . HTTP.Simple.setRequestMethod "PUT"
  . HTTP.Simple.setRequestPath (Text.Encoding.encodeUtf8 ("/" <> name))
  $ baseRequest

deleteIndex :: Text.Text -> IO ()
deleteIndex name
  = sendRequest_
  . HTTP.Simple.setRequestMethod "DELETE"
  . HTTP.Simple.setRequestPath (Text.Encoding.encodeUtf8 ("/" <> name))
  $ baseRequest

indexDocument :: Aeson.ToJSON a => Text.Text -> Text.Text -> Text.Text -> a -> IO ()
indexDocument indexName typeName documentId document
  = sendRequest_
  . HTTP.Simple.setRequestBodyJSON document
  . HTTP.Simple.setRequestMethod "PUT"
  . HTTP.Simple.setRequestPath (Text.Encoding.encodeUtf8 ("/" <> indexName <> "/" <> typeName <> "/" <> documentId))
  $ baseRequest

analyzeText :: Text.Text -> Text.Text -> Text.Text -> IO ()
analyzeText indexName analyzerName text
  = sendRequest_
  . HTTP.Simple.setRequestQueryString
    [ ("analyzer", (Just . Text.Encoding.encodeUtf8) analyzerName)
    , ("text", (Just . Text.Encoding.encodeUtf8) text)
    ]
  . HTTP.Simple.setRequestPath (Text.Encoding.encodeUtf8 ("/" <> indexName <> "/_analyze"))
  $ Common.baseRequest

sendRequest_ :: HTTP.Simple.Request -> IO ()
sendRequest_ request = do
  _ <- sendRequest request
  return ()

sendRequest :: HTTP.Simple.Request -> IO (HTTP.Simple.Response Aeson.Value)
sendRequest request = do
  print request
  response <- HTTP.Simple.httpJSON request
  Common.pPrintResponse response
  return response
