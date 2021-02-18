module Main where

import Prelude
import Effect.Console (log)

data X = X

class Y a where
  member :: a -> Unit

instance yX :: Y X where
  member X = unit

class (Y a) <= Z a

instance zX :: Z X

test1 :: Unit
test1 = member X

f :: forall a. Y a ? a -> Unit
f a = unit

g :: forall a. Z a ? a -> Unit
g a = f a

h :: forall a. Z a => a -> Unit
h a = f a

test2 :: Unit
test2 = g X <> h X

main = log "Done"
