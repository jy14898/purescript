-- @shouldFailWith NoInstanceFound
module Main where

import Prelude
import Effect.Console (log)

data X = X

class Y a where
  member :: a -> Unit

instance yX :: Y X where
  member X = unit

test1 :: forall a. Y a ? a -> Unit
test1 a = member a

main = log "Done"
