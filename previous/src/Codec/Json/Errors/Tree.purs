module Codec.Json.Errors.Tree where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Newtype (class Newtype)
import Data.These (These(..))
import Data.Traversable (class Traversable)

-- | Tree-like data structure where `Left` indicates the branches
-- | and `Right` indicates the leaves.
-- | "Context-less" branches are appended together.
newtype TreeError context leafError =
  TreeError (Either (These context (NonEmptyArray (TreeError context leafError))) leafError)

derive instance (Eq context, Eq leafError) => Eq (TreeError context leafError)
derive instance Newtype (TreeError context leafError) _

instance Semigroup (TreeError context leafError) where
  append = case _, _ of
    TreeError (Left (That x)), TreeError (Left (That y)) ->
      TreeError $ Left $ That $ x <> y
    x@(TreeError (Right _)), TreeError (Left (That y)) ->
      TreeError $ Left $ That $ NEA.cons x y
    TreeError (Left (That x)), y@(TreeError (Right _)) ->
      TreeError $ Left $ That $ NEA.snoc x y
    l, r ->
      TreeError $ Left $ That $ NEA.cons' l [ r ]

derive instance Functor (TreeError context)
derive instance Foldable (TreeError context)
derive instance Bifoldable TreeError
derive instance Traversable (TreeError context)
derive instance Bifunctor TreeError
derive instance Bitraversable TreeError
