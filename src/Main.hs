module Main where

import qualified TMDB
import qualified Tokens

main :: IO ()
main = do
  TMDB.runAll
  Tokens.runAll
  return ()
