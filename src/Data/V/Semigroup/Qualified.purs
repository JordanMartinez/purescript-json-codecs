module Data.V.Semigroup.Qualified where

import Data.Validation.Semigroup (V, andThen)

bind :: forall err a b. V err a -> (a -> V err b) -> V err b
bind = andThen
