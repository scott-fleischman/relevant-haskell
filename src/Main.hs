module Main where

import qualified TMDB

main :: IO ()
main = do
  TMDB.runAll
  return ()
