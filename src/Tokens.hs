{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Code for Chapter 4
module Tokens where

import qualified Common
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.QQ       as Aeson.QQ
import qualified Data.ByteString     as ByteString
import           Data.Semigroup      ((<>))
import qualified Network.HTTP.Simple as HTTP.Simple

runAll :: IO ()
runAll = do
  -- IBM acronym
  Tokens.createExampleFilter
  Tokens.testExampleFilter

  -- phone number
  Tokens.createMyLibraryFilter
  Tokens.testMyLibraryFilter

-- Section 4.1.1 Acronyms
createExampleFilter :: IO ()
createExampleFilter = createFilter "example" [Aeson.QQ.aesonQQ|
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

testFilter :: ByteString.ByteString -> [(ByteString.ByteString, Maybe ByteString.ByteString)] -> IO ()
testFilter name queryString = do
  let
    request
      = HTTP.Simple.setRequestQueryString queryString
      . HTTP.Simple.setRequestPath ("/" <> name <> "/_analyze")
      $ Common.baseRequest
  response :: HTTP.Simple.Response Aeson.Value <- HTTP.Simple.httpJSON request
  Common.pPrintResponse response

-- Section 4.1.1 Phone numbers
testExampleFilter :: IO ()
testExampleFilter = testFilter "example"
  [ ("analyzer", Just "standard_with_acronyms")
  , ("text", Just "I.B.M. versus IBM versus ibm")
  ]

createFilter :: Aeson.ToJSON a => ByteString.ByteString -> a -> IO ()
createFilter name body = do
  let
    request
      = HTTP.Simple.setRequestBodyJSON body
      . HTTP.Simple.setRequestMethod "PUT"
      . HTTP.Simple.setRequestPath ("/" <> name)
      $ Common.baseRequest

  response :: HTTP.Simple.Response Aeson.Value <- HTTP.Simple.httpJSON request
  Common.pPrintResponse response

createMyLibraryFilter :: IO ()
createMyLibraryFilter = createFilter "my_library" $ [Aeson.QQ.aesonQQ|
{
  "settings": {
    "analysis": {
      "filter": {
        "phone_num_filter": {
          "type": "word_delimiter",
          "catenate_all": true,
          "generate_number_parts": false
        },
        "phone_num_parts": {
          "type": "pattern_capture",
          "patterns": ["(\\d{7}$)", "(\\d{10}$)"],
          "preserve_original": true
        }
      },
      "analyzer": {
        "phone_num": {
          "tokenizer": "keyword",
          "filter": ["phone_num_filter", "phone_num_parts"]
        }
      }
    }
  }
}
  |]

testMyLibraryFilter :: IO ()
testMyLibraryFilter = testFilter "my_library"
  [ ("analyzer", Just "phone_num")
  , ("text", Just "1(800)867-5309")
  ]
