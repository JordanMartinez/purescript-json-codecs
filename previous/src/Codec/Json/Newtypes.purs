module Codec.Json.Newtypes where

import Prelude

import Data.Newtype (class Newtype)

-- | Used in type class-based codecs to indicate that a record field
-- | may or may not exist.
newtype Optional a = Optional a

derive instance Eq a => Eq (Optional a)
derive instance Ord a => Ord (Optional a)
instance Show a => Show (Optional a) where
  show (Optional a) = "Optional(" <> show a <> ")"

derive instance Newtype (Optional a) _

-- | Enables local overrides for types with kind `Type`
newtype K0 :: Symbol -> Type -> Type
newtype K0 sym a = K0 a

derive instance Eq a => Eq (K0 sym a)
derive instance Ord a => Ord (K0 sym a)
instance Show a => Show (K0 sym a) where
  show (K0 a) = "K0(" <> show a <> ")"

derive instance Newtype (K0 sym a) _

-- | Enables local overrides for types with kind `Type -> Type`
newtype K1 :: Symbol -> Type -> Type
newtype K1 sym a = K1 a

derive instance Eq a => Eq (K1 sym a)
derive instance Ord a => Ord (K1 sym a)
instance Show a => Show (K1 sym a) where
  show (K1 a) = "K1(" <> show a <> ")"

derive instance Newtype (K1 sym a) _

-- | Enables local overrides for types with kind `Type -> Type -> Type`
newtype K2 :: Symbol -> Type -> Type
newtype K2 sym a = K2 a

derive instance Eq a => Eq (K2 sym a)
derive instance Ord a => Ord (K2 sym a)
instance Show a => Show (K2 sym a) where
  show (K2 a) = "K2(" <> show a <> ")"

derive instance Newtype (K2 sym a) _

-- | Enables local overrides for types with kind `Type -> Type -> Type -> Type`
newtype K3 :: Symbol -> Type -> Type
newtype K3 sym a = K3 a

derive instance Eq a => Eq (K3 sym a)
derive instance Ord a => Ord (K3 sym a)
instance Show a => Show (K3 sym a) where
  show (K3 a) = "K3(" <> show a <> ")"

derive instance Newtype (K3 sym a) _

-- | Used in bidirectional class-based codecs to add hints to `Variant` codecs
newtype WithType :: Symbol -> Type -> Type
newtype WithType sym a = WithType a

derive instance Eq a => Eq (WithType sym a)
derive instance Ord a => Ord (WithType sym a)
derive instance Newtype (WithType sym a) _

-- | Used in bidirectional class-based codecs to add hints to `Variant` codecs
newtype WithCtor :: Symbol -> Type -> Type
newtype WithCtor sym a = WithCtor a

derive instance Eq a => Eq (WithCtor sym a)
derive instance Ord a => Ord (WithCtor sym a)
derive instance Newtype (WithCtor sym a) _

-- | Used in bidirectional class-based codecs to add hints to `Variant` codecs
newtype WithSubterm :: Int -> Type -> Type
newtype WithSubterm int a = WithSubterm a

derive instance Eq a => Eq (WithSubterm int a)
derive instance Ord a => Ord (WithSubterm int a)
derive instance Newtype (WithSubterm int a) _

-- | Used in bidirectional class-based codecs to add hints to `Variant` codecs
newtype WithField :: Symbol -> Type -> Type
newtype WithField sym a = WithField a

derive instance Eq a => Eq (WithField sym a)
derive instance Ord a => Ord (WithField sym a)
derive instance Newtype (WithField sym a) _
