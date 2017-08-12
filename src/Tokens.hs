{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Code for Chapter 4
module Tokens where

import qualified Common
import qualified Control.Monad       as Monad
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.QQ       as Aeson.QQ
import qualified Data.ByteString     as ByteString
import           Data.Semigroup      ((<>))
import qualified Data.Text           as Text
import qualified Network.HTTP.Simple as HTTP.Simple

runAll :: IO ()
runAll = do
  standardCloneAnalyzer
  englishCloneAnalyzer

  -- this requires the analysis-phonetic plugin
  -- https://www.elastic.co/guide/en/elasticsearch/plugins/current/analysis-phonetic.html
  Monad.when False $ do
    phoneticAnalyzer

  singleField

  acronymAnalyzer

  Monad.when False $ do
    createPhoneNumberAnalyzer
    testPhoneNumberAnalyzer

  return ()

-- Listing 4.1 Recreating the standard analyzer
standardCloneAnalyzer :: IO ()
standardCloneAnalyzer = do
  let
    indexName = "my_library"
    analyzerName = "standard_clone"
    analyzerNameString = Text.unpack analyzerName
  Common.printHeader $ analyzerName <> " analyzer"
  Common.deleteIndex indexName
  Common.createIndex indexName [Aeson.QQ.aesonQQ|
{
  "settings": {
    "analysis": {
      "analyzer": {
        $analyzerNameString: {
          "tokenizer": "standard",
          "filter": [
            "standard",
            "lowercase",
            "stop"
          ]
        }
      }
    }
  }
}
    |]
  Common.analyzeText indexName analyzerName "Dr. Strangelove: Or How I Learned to Stop Worrying and Love the Bomb"

-- Listing 4.2 Recreating the English analyzer
englishCloneAnalyzer :: IO ()
englishCloneAnalyzer = do
  let
    indexName = "my_library"
    analyzerName = "english_clone"
    analyzerNameString = Text.unpack analyzerName
  Common.printHeader $ analyzerName <> " analyzer"
  Common.deleteIndex indexName
  Common.createIndex "my_library" [Aeson.QQ.aesonQQ|
{
  "settings": {
    "analysis": {
      "filter": {
        "english_stop": {
          "type": "stop",
          "stopwords": "_english_"
        },
        "english_keywords": {
          "type": "keyword_marker",
          "keywords": ["skies"]
        },
        "english_stemmer": {
          "type": "stemmer",
          "language": "english"
        },
        "english_possessive_stemmer": {
          "type": "stemmer",
          "language": "possessive_english"
        }
      },
      "analyzer": {
        $analyzerNameString: {
          "tokenizer": "standard",
          "filter": [
            "english_possessive_stemmer",
            "lowercase",
            "english_stop",
            "english_keywords",
            "english_stemmer"
          ]
        }
      }
    }
  }
}
  |]

  Common.analyzeText indexName analyzerName "flower flowers flowering flowered flower"
  Common.analyzeText indexName analyzerName "silly silliness sillied sillying"

-- Listing 4.4
phoneticAnalyzer :: IO ()
phoneticAnalyzer = do
  let
    indexName = "my_library"
    analyzerName = "phonetic"
    analyzerNameString = Text.unpack analyzerName
  Common.printHeader $ analyzerName <> " analyzer"
  Common.deleteIndex indexName
  Common.createIndex indexName [Aeson.QQ.aesonQQ|
{
  "settings": {
    "analysis": {
      "analyzer": {
        $analyzerNameString: {
          "tokenizer": "standard",
          "filter": [
            "standard",
            "lowercase",
            "my_doublemetaphone"
          ]
        }
      },
      "filter": {
        "my_doublemetaphone": {
          "type": "phonetic",
          "encoder": "doublemetaphone",
          "replace": true
        }
      }
    }
  }
}
  |]

  Common.analyzeText indexName analyzerName "message from Dalai Lama"
  Common.analyzeText indexName analyzerName "message from tall llama"

singleField :: IO ()
singleField = do
  let
    indexName = "my_library"
    typeName = "example"
  Common.printHeader "single field"
  Common.deleteIndex indexName
  let
    defaultSettings = [Aeson.QQ.aesonQQ|
{
  "settings": {
    "number_of_shards": 1
  }
}
    |]
    _englishSettings = [Aeson.QQ.aesonQQ|
{
  "settings": {
    "number_of_shards": 1
  },
  "mappings": {
    "example": {
      "properties": {
        "title": {
          "type":     "string",
          "analyzer": "english"
        }
      }
    }
  }
}
    |]
  Common.createIndex indexName defaultSettings

  Common.indexDocument indexName typeName "1" [Aeson.QQ.aesonQQ| { "title":"apple apple apple apple apple"} |]
  Common.indexDocument indexName typeName "2" [Aeson.QQ.aesonQQ| { "title":"apple apple apple banana banana"} |]
  Common.indexDocument indexName typeName "3" [Aeson.QQ.aesonQQ| { "title":"apple banana blueberry coconut"} |]
  Common.refreshIndex indexName

  let doSearch = Common.searchJSON indexName typeName [Aeson.QQ.aesonQQ|
{
  "explain": "true",
  "query": {
    "match": {
      "title": "apple"
    }
  }
}
    |]
  doSearch

  Common.indexDocument indexName typeName "4" [Aeson.QQ.aesonQQ| { "title":"apples apple"} |]
  Common.refreshIndex indexName

  doSearch

-- Section 4.1.1 Acronyms
acronymAnalyzer :: IO ()
acronymAnalyzer = do
  let
    indexName = "example"
    analyzerName = "standard_with_acronyms"
    analyzerNameString = Text.unpack analyzerName
  Common.printHeader $ analyzerName <> " analyzer"
  Common.deleteIndex indexName
  Common.createIndex indexName [Aeson.QQ.aesonQQ|
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
        $analyzerNameString: {
          "tokenizer": "standard",
          "filter": ["standard", "lowercase", "acronyms"]
        }
      }
    }
  }
}
  |]

  Common.analyzeText indexName analyzerName "I.B.M. versus IBM versus ibm"

  -- Section 4.1.1 Phone numbers
createPhoneNumberAnalyzer :: IO ()
createPhoneNumberAnalyzer = createAnalyzer "my_library" $ [Aeson.QQ.aesonQQ|
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

testPhoneNumberAnalyzer :: IO ()
testPhoneNumberAnalyzer = testAnalyzer "my_library"
  [ ("analyzer", Just "phone_num")
  , ("text", Just "1(800)867-5309")
  ]

createRetailAnalyzer :: IO ()
createRetailAnalyzer = createAnalyzer "retail" $ [Aeson.QQ.aesonQQ|
{
  "settings": {
    "analysis": {
      "filter": {
        "retail_syn_filter": {
          "type": "synonym",
          "synonyms": [
            "dress shoe,dress shoes => dress_shoe, shoe"
          ]
        }
      },
      "analyzer": {
        "retail_analyzer": {
          "tokenizer": "standard",
          "filter": [
            "english_possessive_stemmer",
            "lowercase",
            "retail_syn_filter",
            "english_keywords",
            "english_stemmer"
          ]
        }
      }
    }
  },
  "mappings": {
    "items": {
      "properties": {
        "desc": {
          "type": "string",
          "analyzer": "retail_analyzer"
        }
      }
    }
  }
}
  |]

createAnalyzer :: Aeson.ToJSON a => ByteString.ByteString -> a -> IO ()
createAnalyzer indexName body = do
  let
    request
      = HTTP.Simple.setRequestBodyJSON body
      . HTTP.Simple.setRequestMethod "PUT"
      . HTTP.Simple.setRequestPath ("/" <> indexName)
      $ Common.baseRequest

  response :: HTTP.Simple.Response Aeson.Value <- HTTP.Simple.httpJSON request
  Common.pPrintResponse response

testAnalyzer :: ByteString.ByteString -> [(ByteString.ByteString, Maybe ByteString.ByteString)] -> IO ()
testAnalyzer indexName queryString = do
  let
    request
      = HTTP.Simple.setRequestQueryString queryString
      . HTTP.Simple.setRequestPath ("/" <> indexName <> "/_analyze")
      $ Common.baseRequest
  response :: HTTP.Simple.Response Aeson.Value <- HTTP.Simple.httpJSON request
  Common.pPrintResponse response
