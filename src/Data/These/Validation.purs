module Data.These.Validation where

import Prelude

import Control.Alt (class Alt)
import Control.Extend (class Extend)
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe, isJust)
import Data.Newtype (class Newtype)
import Data.These (These(..), both, fromThese, maybeThese, that, thatOrBoth, these, theseLeft, theseRight, this, thisOrBoth)
import Data.These as These
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple)
import Safe.Coerce (coerce)

-- | Similiar to `Data.Validation.Semigroup.V` but handles
-- | errors differently when using `<|>`
-- |
-- | `This` = error
-- | `That` = success
-- | `Both` = errors before success and the successful value
-- |
-- | See the `Semigroup` instance docs.
newtype Validation err a = Validation (These err a)

derive newtype instance (Eq err, Eq a) => Eq (Validation err a)

derive instance Newtype (Validation err a) _

-- | Very similar to `Data.Validation.Semigroup.V`.
-- |
-- | Like `V`, getting a failure will not short-cirtcuit a monadic computation.
-- | Rather, all such errors will accumulate and then be reported at the end.
-- |
-- | The difference lies in what happens when one has
-- | multiple errors before getting a valid value. For example,
-- | 
-- | ```
-- |   foo = getA <|> getB <|> getC
-- | ```
-- |
-- | Let's say `getA` and `getB` produce errors whereas `getC`
-- | produces a successful value. Unlike `V`, this data type
-- | will store the errors from `getA` and `getB`, so that one understands
-- | why `foo` has the value returned by `getC` rather than
-- | `getA` or `getB`.
instance Semigroup err => Semigroup (Validation err a) where
  append (Validation l) (Validation r) = Validation case l of
    This a -> case r of
      This b -> This $ a <> b
      That y -> Both a y
      Both x y -> Both (a <> x) y
    That x -> That x
    Both a x -> Both a x

instance Semigroup err => Alt (Validation err) where
  alt = append

derive newtype instance Functor (Validation err)

derive newtype instance Foldable (Validation err)

derive newtype instance Traversable (Validation err)

derive newtype instance Bifunctor Validation

derive newtype instance Bifoldable Validation

derive newtype instance Bitraversable Validation

derive newtype instance Semigroup err => Apply (Validation err)

derive newtype instance Semigroup err => Applicative (Validation err)

derive newtype instance Semigroup err => Bind (Validation err)

derive newtype instance Semigroup err => Monad (Validation err)

derive newtype instance Extend (Validation err)

instance (Show err, Show a) => Show (Validation err a) where
  show (Validation x) = "Validation( " <> show x <> ")"

fail :: forall err a. err -> Validation err a
fail = Validation <<< This

succeed :: forall err a. a -> Validation err a
succeed = Validation <<< That

validation :: forall a b c. (a -> c) -> (b -> c) -> (a -> b -> c) -> Validation a b -> c
validation l r lr (Validation t) = these l r lr t

failOrBoth :: forall a b. a -> Maybe b -> Validation a b
failOrBoth a val = Validation $ thisOrBoth a val

successOrBoth :: forall a b. b -> Maybe a -> Validation a b
successOrBoth a val = Validation $ thatOrBoth a val

maybeValidation :: forall a b. Maybe a -> Maybe b -> Maybe (Validation a b)
maybeValidation l r = coerce $ maybeThese l r

fromValidation :: forall a b. a -> b -> Validation a b -> Tuple a b
fromValidation a x (Validation t) = fromThese a x t

validationErr :: forall a b. Validation a b -> Maybe a
validationErr (Validation t) = theseLeft t

validationSuccess :: forall a b. Validation a b -> Maybe b
validationSuccess (Validation t) = theseRight t

-- | Returns the `a` value if and only if the value is constructed with `This`.
errOnly :: forall a b. Validation a b -> Maybe a
errOnly (Validation t) = this t

-- | Returns the `b` value if and only if the value is constructed with `That`.
successOnly :: forall a b. Validation a b -> Maybe b
successOnly (Validation t) = that t

-- | Returns the `a` and `b` values if and only if they are constructed
-- | with `Both`.
successBoth :: forall a b. Validation a b -> Maybe (Tuple a b)
successBoth (Validation t) = both t

-- | Returns `true` when the `These` value is `This`
isErrOnly :: forall a b. Validation a b -> Boolean
isErrOnly = isJust <<< errOnly

-- | Returns `true` when the `These` value is `That`
isSuccessOnly :: forall a b. Validation a b -> Boolean
isSuccessOnly = isJust <<< successOnly

-- | Returns `true` when the `These` value is `Both`
isSuccessBoth :: forall a b. Validation a b -> Boolean
isSuccessBoth = isJust <<< successBoth

-- | Returns `true` when `That` or `Both`
isSuccessful :: forall a b. Validation a b -> Boolean
isSuccessful (Validation t) = case t of
  That _ -> true
  Both _ _ -> true
  _ -> false

-- | Returns `true` when `This` or `Both`
hasError :: forall a b. Validation a b -> Boolean
hasError (Validation t) = case t of
  This _ -> true
  Both _ _ -> true
  _ -> false

-- | Swap between `This` and `That`, and flips the order for `Both`.
swap :: forall a b. Validation a b -> Validation b a
swap (Validation t) = Validation $ These.swap t

-- | Re-associate `These` from left to right.
assoc :: forall a b c. Validation (Validation a b) c -> Validation a (Validation b c)
assoc (Validation t) = Validation case t of
  This (Validation (This a)) -> This a
  This (Validation (That b)) -> That $ Validation $ This b
  This (Validation (Both a b)) -> Both a $ Validation $ This b
  That c -> That $ Validation $ That c
  Both (Validation (This a)) c -> Both a $ Validation $ That c
  Both (Validation (That b)) c -> That $ Validation $ Both b c
  Both (Validation (Both a b)) c -> Both a $ Validation $ Both b c
