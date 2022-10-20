module Json.Types where

import Prelude

import Data.Newtype (class Newtype)

newtype Optional a = Optional a

derive instance Eq a => Eq (Optional a)
derive instance Ord a => Ord (Optional a)
instance Show a => Show (Optional a) where
  show (Optional a) = "Optional(" <> show a <> ")"

derive instance Newtype (Optional a) _
