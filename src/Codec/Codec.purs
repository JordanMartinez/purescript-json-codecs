-- | Note: this module is the same as `purescript-codec@v6.0.0`
-- | with the following changes:
-- | - `Codec`'s `decode` and `encode` functions were converted to an uncurried representation via `Fn*` types
-- | - The decode function includes additional parts that enable custom decoding errors.
-- | - The decoding monad was hard-coded to `V e`
module Codec.Codec where

import Prelude hiding (compose, identity)

import Codec.Decoder (DecoderFn(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, mkFn2, mkFn5, runFn2, runFn5)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Tuple (Tuple(..), fst)
import Data.Validation.Semigroup (V(..), andThen)

data Codec decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput decodeOutputEncodeAccumuladecodeToEncodeFromr =
  Codec
    (DecoderFn decodePath decodeHandlers decodeError extra decodeInput decodeOutputEncodeAccumuladecodeToEncodeFromr)
    (Fn2 extra encodeInput (Tuple encodeOutput decodeOutputEncodeAccumuladecodeToEncodeFromr))

instance Functor (Codec decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput) where
  map
    :: forall decodeOutputEncodeAccumuladecodeToEncodeFromr decodeOutputEncodeAccumuladecodeToEncodeFromr'
     . (decodeOutputEncodeAccumuladecodeToEncodeFromr -> decodeOutputEncodeAccumuladecodeToEncodeFromr')
    -> Codec decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput decodeOutputEncodeAccumuladecodeToEncodeFromr
    -> Codec decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput decodeOutputEncodeAccumuladecodeToEncodeFromr'
  map f (Codec g h) = Codec (f <$> g) (mkFn2 \extra a -> f <$> runFn2 h extra a)

instance Invariant (Codec decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput) where
  imap = imapF

instance Semigroup encodeOutput => Apply (Codec decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput) where
  apply (Codec f g) (Codec h i) = Codec (f <*> h)
    ( mkFn2 \extra a ->
        runFn2 g extra a <*> runFn2 i extra a
    )

instance Monoid encodeOutput => Applicative (Codec decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput) where
  pure x = Codec (pure x) (mkFn2 \_ _ -> pure x)

instance Profunctor (Codec decodePath decodeHandlers decodeError extra decodeInput encodeOutput) where
  dimap f g (Codec h i) =
    Codec (g <$> h) (mkFn2 \extra a -> g <$> runFn2 i extra (f a))

codec
  ∷ ∀ decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput
  . (DecoderFn decodePath decodeHandlers decodeError extra decodeInput encodeInput)
  → (Fn2 extra encodeInput encodeOutput)
  → Codec decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput encodeInput
codec f g = Codec f (mkFn2 \extra b -> Tuple (runFn2 g extra b) b)

type Codec' decodePath decodeHandlers decodeError extra decodeFromEncodeTo decodeToEncodeFrom =
  Codec decodePath decodeHandlers decodeError extra decodeFromEncodeTo decodeFromEncodeTo decodeToEncodeFrom decodeToEncodeFrom

codec'
  ∷ ∀ decodePath decodeHandlers decodeError extra decodeFromEncodeTo decodeToEncodeFrom
  . (DecoderFn decodePath decodeHandlers decodeError extra decodeFromEncodeTo decodeToEncodeFrom)
  → (Fn2 extra decodeToEncodeFrom decodeFromEncodeTo)
  → Codec' decodePath decodeHandlers decodeError extra decodeFromEncodeTo decodeToEncodeFrom
codec' = codec

decode
  ∷ ∀ decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput decodeOutputEncodeAccumuladecodeToEncodeFromr
  . Codec decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput decodeOutputEncodeAccumuladecodeToEncodeFromr
  → DecoderFn decodePath decodeHandlers decodeError extra decodeInput decodeOutputEncodeAccumuladecodeToEncodeFromr
decode (Codec f _) = f

encode
  ∷ ∀ decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput decodeOutputEncodeAccumuladecodeToEncodeFromr
  . Codec decodePath decodeHandlers decodeError extra decodeInput encodeOutput encodeInput decodeOutputEncodeAccumuladecodeToEncodeFromr
  → Fn2 extra encodeInput (Tuple encodeOutput decodeOutputEncodeAccumuladecodeToEncodeFromr)
encode (Codec _ f) = f

identity
  ∷ ∀ decodePath decodeHandlers decodeError extra a
  . Codec decodePath decodeHandlers decodeError extra a a a a
identity = codec (DecoderFn $ mkFn5 \_ _ _ _ a -> V $ Right a) (mkFn2 \_ a -> a)

compose
  ∷ ∀ decodePath decodeHandlers decodeError extra a d f b e c
  . Codec decodePath decodeHandlers decodeError extra d c e f
  → Codec decodePath decodeHandlers decodeError extra a b c d
  → Codec decodePath decodeHandlers decodeError extra a b e f
compose (Codec (DecoderFn f) g) (Codec (DecoderFn h) i) =
  Codec
    ( DecoderFn $ mkFn5 \path appendFn handlers extra decodeFromEncodeToValue ->
        andThen
          (runFn5 h path appendFn handlers extra decodeFromEncodeToValue)
          (\x -> runFn5 f path appendFn handlers extra x)
    )
    (mkFn2 \extra decodeFromEncodeToValue -> lmap (\x -> fst $ runFn2 i extra x) $ runFn2 g extra decodeFromEncodeToValue)

infixr 8 compose as <~<

composeFlipped
  ∷ ∀ decodePath decodeHandlers decodeError extra a d f b e c
  . Codec decodePath decodeHandlers decodeError extra a b c d
  → Codec decodePath decodeHandlers decodeError extra d c e f
  → Codec decodePath decodeHandlers decodeError extra a b e f
composeFlipped = flip compose

infixr 8 composeFlipped as >~>

-- | `Codec` is defined as a `Profunctor` so that `lcmap` can be used to target
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
