{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Monad.IO.Class as Monad.IO
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as ByteString.Lazy
import qualified Data.HashMap.Lazy      as HashMap.Lazy
import qualified Data.Text              as Text
import qualified Data.Vector            as Vector
import qualified Database.V5.Bloodhound as Bloodhound
import qualified Network.HTTP.Client    as HTTP.Client

main :: IO ()
main = extract >>= reindex

extract :: IO Aeson.Object
extract = do
  putStrLn $ "Loading " ++ tmdbPath
  bytes <- ByteString.Lazy.readFile tmdbPath
  tmdb :: HashMap.Lazy.HashMap Text.Text Aeson.Value <-
    case Aeson.eitherDecode bytes of
      Left err -> fail err
      Right x  -> return x
  putStrLn $ (show . HashMap.Lazy.size) tmdb ++ " movies loaded"
  return tmdb

reindex :: Aeson.Object -> IO ()
reindex tmdb = Bloodhound.withBH HTTP.Client.defaultManagerSettings server $ do
  Monad.IO.liftIO $ putStrLn "Deleting index..."
  _ <- Bloodhound.deleteIndex tmdbIndex

  Monad.IO.liftIO $ putStrLn "Creating index..."
  let indexSettings = Bloodhound.IndexSettings (Bloodhound.ShardCount 1) (Bloodhound.ReplicaCount 0)
  _ <- Bloodhound.createIndex indexSettings tmdbIndex

  Monad.IO.liftIO $ putStrLn "Bulk indexing documents..."
  let
    makeOperation (movieId, movie) = Bloodhound.BulkIndex tmdbIndex movieMapping (Bloodhound.DocId movieId) movie
    operations = Vector.fromList . fmap makeOperation . HashMap.Lazy.toList $ tmdb
  _ <- Bloodhound.bulk operations

  return ()

search :: Bloodhound.Search -> IO ()
search query = Bloodhound.withBH HTTP.Client.defaultManagerSettings server $ do
  Monad.IO.liftIO $ putStrLn "Running query..."
  response <- Bloodhound.searchByType tmdbIndex movieMapping query
  let
    bytes = HTTP.Client.responseBody response
  searchResult <- case Aeson.eitherDecode bytes of
    Left err -> fail $ "Unable to decode response: " ++ err
    Right x  -> return (x :: Bloodhound.SearchResult Aeson.Object)
  let hits = Bloodhound.hits . Bloodhound.searchHits $ searchResult

  Monad.IO.liftIO $ putStrLn "Num\tRelevance Score\t\tMovie Title"
  let
    showMaybe :: Show a => Maybe a -> String
    showMaybe Nothing  = ""
    showMaybe (Just x) = show x

    printHit :: (Int, Bloodhound.Hit Aeson.Object) -> Bloodhound.BH IO ()
    printHit (num, hit) = Monad.IO.liftIO . putStrLn
      $ show num
      ++ "\t" ++ (showMaybe . Bloodhound.hitScore) hit
      ++ "\t" ++ title
      where
      title :: String
      title = case Bloodhound.hitSource hit of
        Nothing -> ""
        Just x ->
          case HashMap.Lazy.lookup "title" x of
            Just (Aeson.String t) -> Text.unpack t
            _                     -> ""

  mapM_ printHit $ zip [1..] hits

  return ()

server :: Bloodhound.Server
server = Bloodhound.Server "http://localhost:9200"

tmdbIndex :: Bloodhound.IndexName
tmdbIndex = Bloodhound.IndexName "tmdb"

movieMapping :: Bloodhound.MappingName
movieMapping = Bloodhound.MappingName "movie"

tmdbPath :: FilePath
tmdbPath = "relevant-search-book/ipython/tmdb.json"
