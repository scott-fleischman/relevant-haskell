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
import qualified Network.HTTP.Simple as HTTP.Simple

runAll :: IO ()
runAll = do
  -- you can use this to comment out blocks
  -- and still compile code without unused warnings
  Monad.when False $ return ()

  Monad.when False $ do
    createStandardCloneAnalyzer
    testStandardCloneAnalyzer

    createEnglishCloneAnalyzer
    testEnglishCloneAnalyzerFlower
    testEnglishCloneAnalyzerSilly

  Monad.when False $ do
    -- this requires the analysis-phonetic plugin
    -- https://www.elastic.co/guide/en/elasticsearch/plugins/current/analysis-phonetic.html
    createPhoneticAnalyzer
    testPhoneticAnalyzerDalaiLama
    testPhoneticAnalyzerTallLlama

  Monad.when False $ do
    createAcronymAnalyzer
    testAcronymAnalyzer

    createPhoneNumberAnalyzer
    testPhoneNumberAnalyzer

  return ()

-- Listing 4.1 Recreating the standard analyzer
createStandardCloneAnalyzer :: IO ()
createStandardCloneAnalyzer = createAnalyzer "my_library" [Aeson.QQ.aesonQQ|
{
  "settings": {
    "analysis": {
      "analyzer": {
        "standard_clone": {
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

testStandardCloneAnalyzer :: IO ()
testStandardCloneAnalyzer = testAnalyzer "my_library"
  [ ("analyzer", Just "standard_clone")
  , ("text", Just "Dr. Strangelove: Or How I Learned to Stop Worrying and Love the Bomb")
  ]

-- Listing 4.2 Recreating the English analyzer
createEnglishCloneAnalyzer :: IO ()
createEnglishCloneAnalyzer = createAnalyzer "my_library" [Aeson.QQ.aesonQQ|
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
          "keywords": []
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
        "english_clone": {
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

testEnglishCloneAnalyzerFlower :: IO ()
testEnglishCloneAnalyzerFlower = testAnalyzer "my_library"
  [ ("analyzer", Just "english_clone")
  , ("text", Just "flower flowers flowering flowered flower")
  ]

testEnglishCloneAnalyzerSilly :: IO ()
testEnglishCloneAnalyzerSilly = testAnalyzer "my_library"
  [ ("analyzer", Just "english_clone")
  , ("text", Just "silly silliness sillied sillying")
  ]

-- Listing 4.4
createPhoneticAnalyzer :: IO ()
createPhoneticAnalyzer = createAnalyzer "my_library" [Aeson.QQ.aesonQQ|
{
  "settings": {
    "analysis": {
      "analyzer": {
        "phonetic": {
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

testPhoneticAnalyzerDalaiLama :: IO ()
testPhoneticAnalyzerDalaiLama = testAnalyzer "my_library"
  [ ("analyzer", Just "phonetic")
  , ("text", Just "message from Dalai Lama")
  ]

testPhoneticAnalyzerTallLlama :: IO ()
testPhoneticAnalyzerTallLlama = testAnalyzer "my_library"
  [ ("analyzer", Just "phonetic")
  , ("text", Just "message from tall llama")
  ]

-- Section 4.1.1 Acronyms
createAcronymAnalyzer :: IO ()
createAcronymAnalyzer = createAnalyzer "example" [Aeson.QQ.aesonQQ|
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

-- Section 4.1.1 Phone numbers
testAcronymAnalyzer :: IO ()
testAcronymAnalyzer = testAnalyzer "example"
  [ ("analyzer", Just "standard_with_acronyms")
  , ("text", Just "I.B.M. versus IBM versus ibm")
  ]

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
