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

-- | Same as `Star` / `extra -> from -> f to`
newtype EncoderFn :: Type -> (Type -> Type) -> Type -> Type -> Type
newtype EncoderFn extra f from to = EncoderFn (Fn2 extra from (f to))

derive instance newtypeEncoderFn :: Newtype (EncoderFn extra f a b) _

instance semigroupoidEncoderFn :: Bind f => Semigroupoid (EncoderFn extra f) where
  compose (EncoderFn f) (EncoderFn g) = EncoderFn $ mkFn2 \extra x -> runFn2 g extra x >>= runFn2 f extra

instance categoryEncoderFn :: Monad f => Category (EncoderFn extra f) where
  identity = EncoderFn $ mkFn2 \_ -> pure

instance functorEncoderFn :: Functor f => Functor (EncoderFn extra f a) where
  map f (EncoderFn g) = EncoderFn $ mkFn2 \extra x -> f <$> runFn2 g extra x

instance invariantEncoderFn :: Invariant f => Invariant (EncoderFn extra f a) where
  imap f g (EncoderFn h) = EncoderFn $ mkFn2 \extra x -> imap f g $ runFn2 h extra x

instance applyEncoderFn :: Apply f => Apply (EncoderFn extra f a) where
  apply (EncoderFn f) (EncoderFn g) = EncoderFn $ mkFn2 \extra x ->
    runFn2 f extra x <*> runFn2 g extra x

instance applicativeEncoderFn :: Applicative f => Applicative (EncoderFn extra f a) where
  pure a = EncoderFn $ mkFn2 \_ _ -> pure a

instance bindEncoderFn :: Bind f => Bind (EncoderFn extra f a) where
  bind (EncoderFn m) f = EncoderFn $ mkFn2 \extra x -> runFn2 m extra x >>= \a -> case f a of EncoderFn g -> runFn2 g extra x

instance monadEncoderFn :: Monad f => Monad (EncoderFn extra f a)

instance altEncoderFn :: Alt f => Alt (EncoderFn extra f a) where
  alt (EncoderFn f) (EncoderFn g) = EncoderFn $ mkFn2 \extra a ->
    runFn2 f extra a <|> runFn2 g extra a

instance plusEncoderFn :: Plus f => Plus (EncoderFn extra f a) where
  empty = EncoderFn $ mkFn2 \_ _ -> empty

instance alternativeEncoderFn :: Alternative f => Alternative (EncoderFn extra f a)

instance monadPlusEncoderFn :: MonadPlus f => MonadPlus (EncoderFn extra f a)

instance distributiveEncoderFn :: Distributive f => Distributive (EncoderFn extra f a) where
  distribute f = EncoderFn $ mkFn2 \extra a -> collect (\(EncoderFn g) -> runFn2 g extra a) f
  collect f = distribute <<< map f

instance profunctorEncoderFn :: Functor f => Profunctor (EncoderFn extra f) where
  dimap f g (EncoderFn ft) = EncoderFn $ mkFn2 \extra a -> g <$> runFn2 ft extra (f a)

instance strongEncoderFn :: Functor f => Strong (EncoderFn extra f) where
  first (EncoderFn f) = EncoderFn $ mkFn2 \extra (Tuple s x) -> map (_ `Tuple` x) (runFn2 f extra s)
  second (EncoderFn f) = EncoderFn $ mkFn2 \extra (Tuple x s) -> map (Tuple x) (runFn2 f extra s)

instance choiceEncoderFn :: Applicative f => Choice (EncoderFn extra f) where
  left (EncoderFn f) = EncoderFn $ mkFn2 \extra a -> either (\a' -> Left <$> runFn2 f extra a') (pure <<< Right) a
  right (EncoderFn f) = EncoderFn $ mkFn2 \extra a -> either (pure <<< Left) (\a' -> Right <$> runFn2 f extra a') a

instance closedEncoderFn :: Distributive f => Closed (EncoderFn extra f) where
  closed (EncoderFn f) = EncoderFn $ mkFn2 \extra g -> distribute (runFn2 f extra <<< g)

hoistEncoderFn :: forall extra f g a b. (f ~> g) -> EncoderFn extra f a b -> EncoderFn extra g a b
hoistEncoderFn f (EncoderFn g) = EncoderFn $ mkFn2 \extra a -> f $ runFn2 g extra a