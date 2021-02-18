module Main where

import Prelude
import Effect.Console (log)

data X = X

class Y a where
  member :: a -> Unit

instance yX :: Y X where
  member X = unit

test1 :: Unit
test1 = member X

f :: forall a. Y a ? a -> Unit
f a = unit

g :: forall a. Y a => a -> Unit
g a = member a

fng :: forall a. Y a => a -> Unit
fng a = f a <> g a

test2 :: Unit
test2 = fng X

main = log "Done"
