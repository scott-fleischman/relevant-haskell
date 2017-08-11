module Main where

import qualified Control.Monad as Monad
import qualified TMDB
import qualified Tokens

main :: IO ()
main = do
  -- you can use this to comment out blocks
  -- and still compile code without unused warnings
  Monad.when False $ return ()

  Monad.when False
    TMDB.runAll

  Tokens.runAll
  return ()
