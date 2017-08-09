{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Monad.IO.Class as Monad.IO
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as ByteString.Lazy
import qualified Data.HashMap.Strict    as HashMap.Strict
import qualified Data.Text              as Text
import qualified Database.V5.Bloodhound as Bloodhound
import qualified Network.HTTP.Client    as HTTP.Client

tmdbPath :: FilePath
tmdbPath = "relevant-search-book/ipython/tmdb.json"

main :: IO ()
main = do
  putStrLn $ "Loading " ++ tmdbPath
  bytes <- ByteString.Lazy.readFile tmdbPath
  tmdb :: HashMap.Strict.HashMap Text.Text Aeson.Value <-
    case Aeson.eitherDecode bytes of
      Left err -> fail err
      Right x  -> return x
  putStrLn $ (show . HashMap.Strict.size) tmdb ++ " movies loaded"

  let
    server = Bloodhound.Server "http://localhost:9200"
    tmdbIndex = Bloodhound.IndexName "tmdb"
  Bloodhound.withBH HTTP.Client.defaultManagerSettings server $ do
    Monad.IO.liftIO $ putStrLn "Deleting index"
    _ <- Bloodhound.deleteIndex tmdbIndex

    Monad.IO.liftIO $ putStrLn "Creating index"
    let indexSettings = Bloodhound.IndexSettings (Bloodhound.ShardCount 1) (Bloodhound.ReplicaCount 0)
    _ <- Bloodhound.createIndex indexSettings tmdbIndex

    return ()
