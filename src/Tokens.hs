{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Code for Chapter 4
module Tokens where

import qualified Common
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.QQ       as Aeson.QQ
import qualified Network.HTTP.Simple as HTTP.Simple

createFilter :: IO ()
createFilter = do
  let
    body = [Aeson.QQ.aesonQQ|
{
  "settings": {
    "analysis": {
      "filter": {
        "acronyms": {
          "type": "word_delimiter",
          "catenate_all": true,
          "generate_word_parts": false,
          "generate_number_parts": false
        }
      },
      "analyzer": {
        "standard_with_acronyms": {
          "tokenizer": "standard",
          "filter": ["standard", "lowercase", "acronyms"]
        }
      }
    }
  }
}
    |]

    request
      = HTTP.Simple.setRequestBodyJSON body
      . HTTP.Simple.setRequestMethod "PUT"
      . HTTP.Simple.setRequestPath "/example"
      $ Common.baseRequest

  response :: HTTP.Simple.Response Aeson.Value <- HTTP.Simple.httpJSON request
  Common.pPrintResponse response

testFilter :: IO ()
testFilter = do
  let
    request
      = HTTP.Simple.setRequestQueryString
        [ ("analyzer", Just "standard_with_acronyms")
        , ("text", Just "I.B.M. versus IBM versus ibm")
        ]
      . HTTP.Simple.setRequestPath "/example/_analyze"
      $ Common.baseRequest
  response :: HTTP.Simple.Response Aeson.Value <- HTTP.Simple.httpJSON request
  Common.pPrintResponse response
