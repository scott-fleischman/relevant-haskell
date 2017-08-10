{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Monad.IO.Class as Monad.IO
import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.QQ          as Aeson.QQ
import qualified Data.ByteString.Lazy   as ByteString.Lazy
import qualified Data.HashMap.Lazy      as HashMap.Lazy
import qualified Data.Maybe             as Maybe
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text.Encoding
import qualified Data.Vector            as Vector
import qualified Database.V5.Bloodhound as Bloodhound
import qualified Extra
import qualified Network.HTTP.Client    as HTTP.Client
import qualified Network.HTTP.Simple    as HTTP.Simple
import qualified Text.Printf            as Printf

main :: IO ()
main = do
  -- extractReindex
  -- search yourFirstSearch
  explainSearch yourFirstSearch
  -- printAnalysis "Fire with Fire"
  return ()

movieMapping :: Aeson.Value
movieMapping = [Aeson.QQ.aesonQQ|
{
  "properties": {
    "title": {
      "type": "string",
      "analyzer": "english"
    },
    "overview": {
      "type": "string",
      "analyzer": "english"
    }
  }
}
  |]

-- Listing 3.6
yourFirstSearch :: Bloodhound.Search
yourFirstSearch =
  let
    usersSearch = Bloodhound.QueryString "basketball with cartoon aliens"
    fields =
      [ Bloodhound.FieldName "title^10"
      , Bloodhound.FieldName "overview"
      ]
    multiMatch = Bloodhound.mkMultiMatchQuery fields usersSearch
    query = Just $ Bloodhound.QueryMultiMatchQuery multiMatch
    searchFilter = Nothing
    baseSearch = Bloodhound.mkSearch query searchFilter
  in baseSearch { Bloodhound.size = Bloodhound.Size 11 }

extractReindex :: IO ()
extractReindex = extract >>= reindex

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
  _ <- Bloodhound.deleteIndex tmdbIndexName

  Monad.IO.liftIO $ putStrLn "Creating index..."
  let indexSettings = Bloodhound.IndexSettings (Bloodhound.ShardCount 1) (Bloodhound.ReplicaCount 0)
  _ <- Bloodhound.createIndex indexSettings tmdbIndexName

  -- comment out to use default mapping
  Monad.IO.liftIO $ putStrLn "Setting mapping..."
  _ <- Bloodhound.putMapping tmdbIndexName movieMappingName movieMapping

  Monad.IO.liftIO $ putStrLn "Bulk indexing documents..."
  let
    makeOperation (movieId, movie) = Bloodhound.BulkIndex tmdbIndexName movieMappingName (Bloodhound.DocId movieId) movie
    operations = Vector.fromList . fmap makeOperation . HashMap.Lazy.toList $ tmdb
  _ <- Bloodhound.bulk operations

  Monad.IO.liftIO $ putStrLn "Refreshing index..."
  _ <- Bloodhound.refreshIndex tmdbIndexName

  return ()

search :: Bloodhound.Search -> IO ()
search query = Bloodhound.withBH HTTP.Client.defaultManagerSettings server $ do
  Monad.IO.liftIO $ putStrLn "Running query..."

  response <- Bloodhound.searchByType tmdbIndexName movieMappingName query
  let
    bytes = HTTP.Client.responseBody response
  searchResult <- case Aeson.eitherDecode bytes of
    Left err -> fail $ "Unable to decode response: " ++ err
    Right x  -> return (x :: Bloodhound.SearchResult Aeson.Object)
  let hits = Bloodhound.hits . Bloodhound.searchHits $ searchResult

  Monad.IO.liftIO $ printHits hits

  return ()

makeSimpleQuery :: Bloodhound.Query -> Aeson.Value
makeSimpleQuery query = Aeson.object [("query", Aeson.toJSON query)]

explainSearch :: Bloodhound.Search -> IO ()
explainSearch searchValue = do
  let
    query = maybe Aeson.Null makeSimpleQuery $ Bloodhound.queryBody searchValue
    request
      = HTTP.Simple.setRequestBodyJSON query
      . HTTP.Simple.setRequestQueryString [("explain", Nothing)]
      . HTTP.Simple.setRequestPath "/tmdb/movie/_validate/query"
      $ baseRequest

  response :: HTTP.Simple.Response Aeson.Value <- HTTP.Simple.httpJSON request
  Extra.pPrintResponse response

printAnalysis :: Text.Text -> IO ()
printAnalysis text = do
  let
    request
      = HTTP.Simple.setRequestBody (HTTP.Client.RequestBodyBS . Text.Encoding.encodeUtf8 $ text)
      . HTTP.Simple.setRequestQueryString [("analyzer", Just "standard")]
      . HTTP.Simple.setRequestPath "/tmdb/_analyze"
      $ baseRequest

  response :: HTTP.Simple.Response Aeson.Value <- HTTP.Simple.httpJSON request
  Extra.pPrintResponse response

baseRequest :: HTTP.Simple.Request
baseRequest
  = HTTP.Simple.setRequestMethod "GET"
  . HTTP.Simple.setRequestPort 9200
  . HTTP.Simple.setRequestHost "localhost"
  $ HTTP.Simple.defaultRequest

printHits :: [Bloodhound.Hit Aeson.Object] -> IO ()
printHits hits = do
  putStrLn "Num\tRelevance Score\t\tMovie Title"
  let
    getScore :: Bloodhound.Hit a -> Double
    getScore hit = Maybe.fromMaybe 0.0 (Bloodhound.hitScore hit)
    getTitle :: Bloodhound.Hit Aeson.Object -> String
    getTitle hit
      | Just source <- Bloodhound.hitSource hit
      , Just (Aeson.String text) <- HashMap.Lazy.lookup "title" source
      = Text.unpack text
    getTitle _ = ""
    printHit :: (Int, Bloodhound.Hit Aeson.Object) -> IO ()
    printHit (num, hit) = Printf.printf "%d\t%0.6f\t\t%s\n" num (getScore hit) (getTitle hit)
  mapM_ printHit $ zip [1..] hits

server :: Bloodhound.Server
server = Bloodhound.Server "http://localhost:9200"

tmdbIndexName :: Bloodhound.IndexName
tmdbIndexName = Bloodhound.IndexName "tmdb"

movieMappingName :: Bloodhound.MappingName
movieMappingName = Bloodhound.MappingName "movie"

tmdbPath :: FilePath
tmdbPath = "relevant-search-book/ipython/tmdb.json"
