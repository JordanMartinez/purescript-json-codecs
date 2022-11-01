module Codec.Encoder where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus, empty)
import Data.Distributive (class Distributive, distribute, collect)
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Functor.Invariant (class Invariant, imap)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Closed (class Closed)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))

newtype StarFn2 :: Type -> (Type -> Type) -> Type -> Type -> Type
newtype StarFn2 extra f from to = StarFn2 (Fn2 extra from (f to))

derive instance newtypeStarFn2 :: Newtype (StarFn2 extra f a b) _

instance semigroupoidStarFn2 :: Bind f => Semigroupoid (StarFn2 extra f) where
  compose (StarFn2 f) (StarFn2 g) = StarFn2 $ mkFn2 \extra x -> runFn2 g extra x >>= runFn2 f extra

instance categoryStarFn2 :: Monad f => Category (StarFn2 extra f) where
  identity = StarFn2 $ mkFn2 \_ -> pure

instance functorStarFn2 :: Functor f => Functor (StarFn2 extra f a) where
  map f (StarFn2 g) = StarFn2 $ mkFn2 \extra x -> f <$> runFn2 g extra x

instance invariantStarFn2 :: Invariant f => Invariant (StarFn2 extra f a) where
  imap f g (StarFn2 h) = StarFn2 $ mkFn2 \extra x -> imap f g $ runFn2 h extra x

instance applyStarFn2 :: Apply f => Apply (StarFn2 extra f a) where
  apply (StarFn2 f) (StarFn2 g) = StarFn2 $ mkFn2 \extra x ->
    runFn2 f extra x <*> runFn2 g extra x

instance applicativeStarFn2 :: Applicative f => Applicative (StarFn2 extra f a) where
  pure a = StarFn2 $ mkFn2 \_ _ -> pure a

instance bindStarFn2 :: Bind f => Bind (StarFn2 extra f a) where
  bind (StarFn2 m) f = StarFn2 $ mkFn2 \extra x -> runFn2 m extra x >>= \a -> case f a of StarFn2 g -> runFn2 g extra x

instance monadStarFn2 :: Monad f => Monad (StarFn2 extra f a)

instance altStarFn2 :: Alt f => Alt (StarFn2 extra f a) where
  alt (StarFn2 f) (StarFn2 g) = StarFn2 $ mkFn2 \extra a ->
    runFn2 f extra a <|> runFn2 g extra a

instance plusStarFn2 :: Plus f => Plus (StarFn2 extra f a) where
  empty = StarFn2 $ mkFn2 \_ _ -> empty

instance alternativeStarFn2 :: Alternative f => Alternative (StarFn2 extra f a)

instance monadPlusStarFn2 :: MonadPlus f => MonadPlus (StarFn2 extra f a)

instance distributiveStarFn2 :: Distributive f => Distributive (StarFn2 extra f a) where
  distribute f = StarFn2 $ mkFn2 \extra a -> collect (\(StarFn2 g) -> runFn2 g extra a) f
  collect f = distribute <<< map f

instance profunctorStarFn2 :: Functor f => Profunctor (StarFn2 extra f) where
  dimap f g (StarFn2 ft) = StarFn2 $ mkFn2 \extra a -> g <$> runFn2 ft extra (f a)

instance strongStarFn2 :: Functor f => Strong (StarFn2 extra f) where
  first (StarFn2 f) = StarFn2 $ mkFn2 \extra (Tuple s x) -> map (_ `Tuple` x) (runFn2 f extra s)
  second (StarFn2 f) = StarFn2 $ mkFn2 \extra (Tuple x s) -> map (Tuple x) (runFn2 f extra s)

instance choiceStarFn2 :: Applicative f => Choice (StarFn2 extra f) where
  left (StarFn2 f) = StarFn2 $ mkFn2 \extra a -> either (\a' -> Left <$> runFn2 f extra a') (pure <<< Right) a
  right (StarFn2 f) = StarFn2 $ mkFn2 \extra a -> either (pure <<< Left) (\a' -> Right <$> runFn2 f extra a') a

instance closedStarFn2 :: Distributive f => Closed (StarFn2 extra f) where
  closed (StarFn2 f) = StarFn2 $ mkFn2 \extra g -> distribute (runFn2 f extra <<< g)

hoistStarFn2 :: forall extra f g a b. (f ~> g) -> StarFn2 extra f a b -> StarFn2 extra g a b
hoistStarFn2 f (StarFn2 g) = StarFn2 $ mkFn2 \extra a -> f $ runFn2 g extra a