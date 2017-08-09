module Extra where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text.Encoding
import qualified Data.Text.Lazy.IO    as Text.Lazy.IO
import qualified Network.HTTP.Client  as HTTP.Client
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
