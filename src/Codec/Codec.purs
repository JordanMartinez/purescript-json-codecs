module Codec.Codec where

import Prelude

import Codec.Decoder (DecoderFn(..))
import Codec.Encoder (EncoderFn(..))
import Control.Alternative (class Alt, class Alternative, class Plus, empty, (<|>))
import Control.Monad.Writer (Writer, execWriter, runWriter, writer)
import Data.Either (Either)
import Data.Function.Uncurried (mkFn2, mkFn5, runFn2, runFn5)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Newtype (un)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Tuple (Tuple(..), fst)
import Data.Validation.Semigroup (V(..), andThen)

-- | A general type for codecs.
data GCodec :: (Type -> Type) -> (Type -> Type -> Type) -> Type -> Type -> Type
data GCodec m n a b = GCodec (m b) (n a b)

instance functorGCodec ∷ (Functor m, Functor (n a)) ⇒ Functor (GCodec m n a) where
  map f (GCodec dec enc) =
    GCodec (map f dec) (map f enc)

instance invariantGCodec ∷ (Functor m, Functor (n a)) ⇒ Invariant (GCodec m n a) where
  imap = imapF

instance applyGCodec ∷ (Apply m, Apply (n a)) ⇒ Apply (GCodec m n a) where
  apply (GCodec decf encf) (GCodec decx encx) =
    GCodec (decf <*> decx) (encf <*> encx)

instance applicativeGCodec ∷ (Applicative m, Applicative (n a)) ⇒ Applicative (GCodec m n a) where
  pure x =
    GCodec (pure x) (pure x)

instance profunctorGCodec ∷ (Functor m, Profunctor n) ⇒ Profunctor (GCodec m n) where
  dimap f g (GCodec dec enc) =
    GCodec (map g dec) (dimap f g enc)

instance altGCodec ∷ (Alt m, Alt (n a)) ⇒ Alt (GCodec m n a) where
  alt (GCodec decx encx) (GCodec decy ency) =
    GCodec (decx <|> decy) (encx <|> ency)

instance plusGCodec ∷ (Plus m, Plus (n a)) ⇒ Plus (GCodec m n a) where
  empty = GCodec empty empty

instance alternativeGCodec ∷ (Alternative m, Alternative (n a)) ⇒ Alternative (GCodec m n a)

instance semigroupoidGCodec ∷ Semigroupoid n ⇒ Semigroupoid (GCodec m n) where
  compose (GCodec decx encx) (GCodec _ ency) =
    GCodec decx (compose encx ency)

-- | Extracts the decoder part of a `GCodec`
decoder ∷ ∀ m n a b. GCodec m n a b → m b
decoder (GCodec f _) = f

-- | Extracts the encoder part of a `GCodec`
encoder ∷ ∀ m n a b. GCodec m n a b → n a b
encoder (GCodec _ f) = f

-- | `GCodec` is defined as a `Profunctor` so that `lcmap` can be used to target
-- | specific fields when defining a codec for a product type. This operator
-- | is a convenience for that:
-- |
-- | ``` purescript
-- | tupleCodec =
-- |   Tuple
-- |     <$> fst ~ fstCodec
-- |     <*> snd ~ sndCodec
-- | ```
infixl 5 lcmap as ~

-- | ```
-- | GCodec 
-- |  (Fn5 a decPath (decE -> decE -> decE) decHandlers decExtra (V decE c))
-- |  (Fn2 encExtra d -> Writer b d)
-- | ```
type Codec decPath decHandlers decE decExtra encExtra a b c d =
  GCodec (DecoderFn a decPath decHandlers decE decExtra) (EncoderFn encExtra (Writer b)) c d

codec ∷ ∀ decPath decHandlers decE decExtra encExtra a b c d. DecoderFn a decPath decHandlers decE decExtra d → EncoderFn encExtra (Writer b) c d → Codec decPath decHandlers decE decExtra encExtra a b c d
codec dec enc = GCodec dec enc

decode
  ∷ ∀ decPath decHandlers decE decExtra encExtra a b c d
   . Codec decPath decHandlers decE decExtra encExtra a b c d
  → a
  -> decPath
  -> (decE -> decE -> decE)
  → decHandlers
  -> decExtra
  -> Either decE d
decode gcodec from path appendFn handlers extra =
  un V $ runFn5 (un DecoderFn (decoder gcodec)) from path appendFn handlers extra

encode
  ∷ ∀ decPath decHandlers decE decExtra encExtra a b c d
   . Codec decPath decHandlers decE decExtra encExtra a b c d
  → encExtra
  -> c
  → b
encode gcodec extra a = execWriter $ runFn2 (un EncoderFn (encoder gcodec)) extra a

mapCodec
  ∷ ∀ decPath decHandlers decE decExtra encExtra a b c d
  . (DecoderFn a decPath decHandlers decE decExtra b)
  → EncoderFn encExtra (Writer b) b a
  → Codec decPath decHandlers decE decExtra encExtra c d a a
  → Codec decPath decHandlers decE decExtra encExtra c d b b
mapCodec (DecoderFn f) (EncoderFn g) (GCodec (DecoderFn decf) (EncoderFn encf)) = GCodec dec enc
  where
  dec = DecoderFn $ mkFn5 \from path appendFn handlers extra -> do
    andThen (runFn5 decf from path appendFn handlers extra) (\a -> runFn5 f a path appendFn handlers extra)
  enc = EncoderFn $ mkFn2 \extra a →
    let
      (Tuple _ x) = runWriter $ runFn2 encf extra (fst $ runWriter $ runFn2 g extra a)
    in
      writer $ Tuple a x

composeCodec
  ∷ ∀ decPath decHandlers decE decExtra encExtra a d f b e c
  . Codec decPath decHandlers decE decExtra encExtra d c e f
  → Codec decPath decHandlers decE decExtra encExtra a b c d
  → Codec decPath decHandlers decE decExtra encExtra a b e f
composeCodec (GCodec (DecoderFn decf) (EncoderFn encf)) (GCodec (DecoderFn decg) (EncoderFn encg)) =
  GCodec
    ( DecoderFn $ mkFn5 \from path appendFn handlers extra →
        andThen (runFn5 decg from path appendFn handlers extra)
          ( \a ->
              runFn5 decf a path appendFn handlers extra
          )
    )
    ( EncoderFn $ mkFn2 \extra c →
        let
          (Tuple w x) = runWriter $ runFn2 encf extra c
        in
          writer $ Tuple w (execWriter $ runFn2 encg extra x)
    )

infixr 8 composeCodec as <~<

composeCodecFlipped
  ∷ ∀ a d f b e c decPath decHandlers decE decExtra encExtra
  . Codec decPath decHandlers decE decExtra encExtra a b c d
  → Codec decPath decHandlers decE decExtra encExtra d c e f
  → Codec decPath decHandlers decE decExtra encExtra a b e f
composeCodecFlipped = flip composeCodec

infixr 8 composeCodecFlipped as >~>

-- hoistCodec ∷ ∀ m m' a b c d. (m ~> m') → Codec m a b c d → Codec m' a b c d
-- hoistCodec f = bihoistGCodec (mapReaderT f) identity

-- | Changes the `m` and `n` functors used in the codec using the specified
-- | natural transformations.
-- bihoistGCodec
--   ∷ ∀ m m' n n' a b
--   . (m ~> m')
--   → (n ~> n')
--   → GCodec m (Star n) a b
--   → GCodec m' (Star n') a b
-- bihoistGCodec f g (GCodec dec (Star h)) = GCodec (f dec) (Star (g <<< h))

type BasicCodec decPath decHandlers decE decExtra encExtra a b = Codec decPath decHandlers decE decExtra encExtra a a b b

-- basicCodec ∷ ∀ m a b. (a → m b) → (b → a) → BasicCodec m a b
-- basicCodec f g = GCodec (ReaderT f) (Star \x → writer $ Tuple x (g x))
