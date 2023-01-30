{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'Voidable' type.
--
-- We encode features via backend-specfic associated type families, and
-- backends which do not support some feature will define it's representative
-- types as uninstantiable (e.g. via 'Void').
--
-- The 'Voidable' type enables defining type class instances for such types
-- without resorting to orphan or overlapping instances.
--
-- The main motivation of introducing this module is to deal with instances of
-- the `HasCodec` type class, which does not currently admit a sensible Void instance.
-- If https://github.com/NorfairKing/autodocodec/issues/31 is resolved, we may be
-- able to dispense with this entire module.
--
-- Example: For some type family 'type T (b :: BackendType)' with a default
-- instantiation 'type T b = Void', we may require e.g. a 'Show (Voidable (T
-- b))' rather than directly 'Show (T b)'.
--
-- This module defines 'instance Show (Voidable Void)', and a backend instance
-- 'SomeBackend' -- with 'type T SomeBackend = SomeBackendT' only has to define
-- 'instance Show (Voidable SomeBackendT)' to participate.
--
-- The price we pay for this encoding is that we have to wrap and unwrap our
-- voidable types explicitly. We also have to explicitly declare instances
-- using 'Voidable', resulting in the boilerplate below, which can cascade to
-- instances over containers containing voidable data as well.
--
-- Note that GHC currently has no way to report if a type class instances is
-- unused. So please do take some care to not define superfluous instances.
module Data.Voidable
  ( Voidable (..),
    coerceCodec',
    coerceCodec,
    voidableCodec,
  )
where

import Autodocodec
import Data.Aeson
import Data.Aeson.Types
import Data.Coerce
import Hasura.Prelude

-- | The 'Voidable' type enables defining type class instances for such types
-- without resorting to orphan or overlapping instances.
newtype Voidable a = Voidable {getVoidable :: a}
  deriving (Generic)

instance Eq (Voidable Void) where
  (Voidable v) == _ = case v of {}

instance Show (Voidable Void) where
  show (Voidable v) = case v of {}

instance ToJSON (Voidable Void) where
  toJSON (Voidable v) = case v of {}

instance FromJSON (Voidable Void) where
  parseJSON _ = fail "Unable to construct Void value"

instance Eq (Voidable [Void]) where
  (Voidable vs) == (Voidable vs') = and (zipWith (==) vs vs')

instance HasCodec (Voidable [Void]) where
  codec = ObjectOfCodec Nothing (pureCodec (Voidable empty))

deriving via
  (Autodocodec (Voidable [Void]))
  instance
    (FromJSON (Voidable [Void]))

deriving via
  (Autodocodec (Voidable [Void]))
  instance
    (ToJSON (Voidable [Void]))

deriving newtype instance Hashable (Voidable Void)

instance FromJSONKey (Voidable Void) where
  fromJSONKey = FromJSONKeyTextParser (parseJSON . String)

instance ToJSONKey (Voidable Void) where
  toJSONKey = toJSONKeyKey (absurd . getVoidable)

deriving newtype instance Eq (Voidable (InsOrdHashMap Void Void))

-- We opt to explicitly deal with 'Voidable' of 'InsOrdHashMap' rather than
-- 'InsOrdHashMap's of 'Voidable's, because we cannot safely coerce
-- 'InsOrdHashMap's as the key type has role 'nominal'.
instance HasCodec (Voidable (InsOrdHashMap Void Void)) where
  codec = ObjectOfCodec Nothing (pureCodec (Voidable mempty))

deriving via
  (Autodocodec (Voidable (InsOrdHashMap Void Void)))
  instance
    FromJSON (Voidable (InsOrdHashMap Void Void))

deriving via
  (Autodocodec (Voidable (InsOrdHashMap Void Void)))
  instance
    ToJSON (Voidable (InsOrdHashMap Void Void))

-- | A 'Codec' is unfortunately not coercible, due to it's type variable roles
-- being inferred as 'nominal' rather than 'representational. Hence we need this
-- function be able to conventiently derive codecs for types wrapped in 'Voidable'
-- (or any other coercible for that matter).
--
--  ghci> :info Codec
--  type role Codec nominal nominal nominal
--  type Codec :: * -> * -> * -> *
--  data Codec context input output where
--
-- and, from the haddocks on Data.Coerce:
--
--  >    If, as a library author of a type constructor like Set a,
--  >    you want to prevent a user of your module to write
--  >    'coerce :: Set T -> Set NT', you need to set the role
--  >    of Set's type parameter to nominal, by writing
--  >
--  >       type role Set nominal
--  >
--  >    For more details about this feature, please refer to Safe Coercions
--  >    by Joachim Breitner, Richard A. Eisenberg, Simon Peyton Jones and
--  >    Stephanie Weirich.
coerceCodec' :: Coercible a b => Codec context a a -> Codec context b b
coerceCodec' = bimapCodec (Right . coerce) coerce

-- | For implementing 'instance HasCodec a => HasCodec (Voidable a)'.
coerceCodec :: forall a b. (HasCodec a, Coercible a b) => JSONCodec b
coerceCodec = coerceCodec' (codec @a)

-- | To help unwrap a voidable codec.
voidableCodec :: forall context a. Codec context (Voidable a) (Voidable a) -> Codec context a a
voidableCodec = bimapCodec (Right . getVoidable) Voidable
