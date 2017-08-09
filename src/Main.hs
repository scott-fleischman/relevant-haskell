{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict  as HashMap.Strict
import qualified Data.Text            as Text

tmdbPath :: FilePath
tmdbPath = "relevant-search-book/ipython/tmdb.json"

main :: IO ()
main = do
  bytes <- ByteString.Lazy.readFile tmdbPath
  tmdb :: HashMap.Strict.HashMap Text.Text Aeson.Value <-
    case Aeson.eitherDecode bytes of
      Left err -> fail err
      Right x  -> return x
  print $ HashMap.Strict.size tmdb
