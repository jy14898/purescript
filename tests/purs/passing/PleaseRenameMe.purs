module Main where

import Prelude
import Effect.Console (log)

data Data a = Data a

-- Testing the forall
instance showData :: forall a. Show a => Show (Data a) where
  show (Data a) = "Data (" <> show a <> ")"

main = do
  log "Done"
