module Codec.Json.Errors.Tree where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldl, bifoldr)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Newtype (class Newtype)
import Data.These (These(..))
import Data.Traversable (class Traversable, sequenceDefault, traverse)

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

instance Functor (TreeError context) where
  map f (TreeError e) = TreeError $ bimap (map (map (map f))) f e

instance Foldable (TreeError context) where
  foldl f z (TreeError e) = foldl f z e
  foldr f z (TreeError e) = foldr f z e
  foldMap f (TreeError e) = foldMap f e

instance Bifoldable TreeError where
  bifoldl f g z (TreeError e) = bifoldl (bifoldl f (foldl (bifoldl f g))) g z e
  bifoldr f g z (TreeError e) = bifoldr (flip (bifoldr f (flip (foldr (flip (bifoldr f g)))))) g z e
  bifoldMap f g (TreeError e) = bifoldMap (bifoldMap f (foldMap (bifoldMap f g))) g e

instance Traversable (TreeError context) where
  traverse f (TreeError e) = TreeError <$> bitraverse (traverse (traverse (traverse f))) f e
  sequence tree = sequenceDefault tree

instance Bifunctor TreeError where
  bimap f g (TreeError e) = TreeError $ bimap (bimap f (map (bimap f g))) g e

instance Bitraversable TreeError where
  bitraverse f g (TreeError e) = TreeError <$> bitraverse (bitraverse f (traverse (bitraverse f g))) g e
  bisequence tree = bisequenceDefault tree
