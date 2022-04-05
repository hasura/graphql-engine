{-# LANGUAGE DeriveAnyClass #-}

module Autodocodec.Extended
  ( disjointEnumCodec,
    HasObjectCodec (..),
    DisjunctCodec (..),
    disjointMatchChoiceCodec,
    disjointMatchChoicesNECodec,
    disjointStringConstCodec,
    TypeAlternative (..),
    sumTypeCodec,
    ValueWrapper (..),
    ValueWrapper2 (..),
    ValueWrapper3 (..),
    module Autodocodec,
  )
where

import Autodocodec
import Control.DeepSeq (NFData)
import Control.Lens (Prism', review, (<&>), (^?))
import Control.Monad (void)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude

-- | A codec for an enum that can be written each with their own codec.
-- Unlike enumCodec, disjointEnumCodec assumes that each provided codec is disjoint.
--
-- === WARNING
--
-- If you don't provide a string for one of the type's constructors, the last codec in the list will be used instead.
disjointEnumCodec ::
  forall enum context.
  Eq enum =>
  NonEmpty (enum, Codec context enum enum) ->
  Codec context enum enum
disjointEnumCodec = go
  where
    go :: NonEmpty (enum, Codec context enum enum) -> Codec context enum enum
    go ((e, c) :| rest) = case NE.nonEmpty rest of
      Nothing -> c
      Just ne -> disjointMatchChoiceCodec c (go ne) $ \i ->
        if e == i
          then Left e
          else Right i

-- | A codec for an enum that can be written as constant string values
-- Unlike stringConstCodec, this function assumes that the provided values and strings are disjoint.
--
-- === Example usage
--
-- >>> data Fruit = Apple | Orange deriving (Show, Eq)
-- >>> let c = disjointStringConstCodec [(Apple, "foo"), (Orange, "bar")]
-- >>> toJSONVia c Orange
-- String "bar"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "foo") :: Maybe Fruit
-- Just Apple
--
-- === WARNING
--
-- If you don't provide a string for one of the type's constructors, the last string in the list will be used instead:
--
-- >>> let c = disjointStringConstCodec [(Apple, "foo")]
-- >>> toJSONVia c Orange
-- String "foo"
disjointStringConstCodec ::
  forall constant.
  Eq constant =>
  NonEmpty (constant, Text) ->
  JSONCodec constant
disjointStringConstCodec =
  disjointEnumCodec
    . NE.map
      ( \(constant, text) ->
          ( constant,
            literalTextValueCodec constant text
          )
      )

-- | Class for types that have a `JSONObjectCodec`, but not necessarily
-- a `JSONValueCodec`.
-- Used for providec codecs for sum type alternatives via `TypeAlternative`.
class HasObjectCodec a where
  objectCodec :: JSONObjectCodec a

requiredTypeField :: Text -> ObjectCodec void ()
requiredTypeField typeName =
  void $ lmapCodec (const typeName) $ requiredFieldWith' "type" $ literalTextCodec typeName

altCodec :: HasObjectCodec a => Text -> Text -> JSONCodec a
altCodec typeName typeFieldValue = object typeName $ requiredTypeField typeFieldValue *> objectCodec

-- | Disjoint version of matchChoiceCodec
disjointMatchChoiceCodec ::
  -- | First codec
  Codec context input output ->
  -- | Second codec
  Codec context input' output ->
  -- | Rendering chooser
  (newInput -> Either input input') ->
  Codec context newInput output
disjointMatchChoiceCodec c1 c2 renderingChooser =
  dimapCodec (either id id) renderingChooser $
    disjointEitherCodec c1 c2

data DisjunctCodec context newInput output where
  DisjunctCodec :: (newInput -> Maybe input) -> Codec context input output -> DisjunctCodec context newInput output

-- | A choice codec for a disjoint non-empty list of options
-- Note that this list of options must be complete.
-- There is a variant of newInput for which a DisjunctCodec is not provided
-- then encoding may fail with a call to `error` (via `fromJust`)
disjointMatchChoicesNECodec ::
  -- | Codecs, each which their own rendering matcher
  NonEmpty (DisjunctCodec context newInput output) ->
  Codec context newInput output
disjointMatchChoicesNECodec l = go l
  where
    go (DisjunctCodec m c :| rest) = case nonEmpty rest of
      Nothing -> lmapCodec (fromJust . m) c
      Just l' ->
        disjointMatchChoiceCodec c (go l') $ \i -> case m i of
          Just j -> Left j
          Nothing -> Right i

-- | Data needed to generate a codec for one alternative of a sum type `a`.
-- The existenstially quantified type `b` represents the object type
-- contained within the alternative.
data TypeAlternative a where
  TypeAlternative ::
    HasObjectCodec b =>
    -- | Name of the object type for the alternative
    Text ->
    -- | Value to require in the "type" field of the object
    Text ->
    -- | Prism to access values of the alternative
    Prism' a b ->
    TypeAlternative a

-- | A codec for a sum type.
-- Note: the list of `TypeAlternative`s must cover all constructors of the sum type
-- Otherwise encoding may fail with a call to error.
-- This is not checked by the compiler.
-- Example:
-- >
-- > data Field
-- >   = ColumnField (ValueWrapper "column" API.V0.ColumnName)
-- >   | RelationshipField RelField
-- >   deriving stock (Eq, Ord, Show, Generic, Data)
-- >
-- > $(makePrisms ''Field)
-- >
-- > instance HasCodec Field where
-- >   codec =
-- >     named "Field" $
-- >       sumTypeCodec
-- >         [ TypeAlternative "ColumnField" "column" _ColumnField,
-- >           TypeAlternative "RelationshipField" "relationship" _RelationshipField
-- >         ]
sumTypeCodec :: NonEmpty (TypeAlternative a) -> JSONCodec a
sumTypeCodec l =
  disjointMatchChoicesNECodec l'
  where
    l' =
      l <&> \(TypeAlternative typeName typeFieldValue p) ->
        DisjunctCodec (^? p) $ review p <$> altCodec typeName typeFieldValue

-- Some wrappers to help with sum types.
-- TODO: can we generalize this using HList or something?
-- TODO: add some usage examples
newtype ValueWrapper (t :: Symbol) a = ValueWrapper {getValue :: a}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (Hashable, NFData)

instance (KnownSymbol t, HasCodec a) => HasObjectCodec (ValueWrapper t a) where
  objectCodec =
    ValueWrapper
      <$> requiredField' (T.pack $ symbolVal (Proxy :: Proxy t)) .= getValue

data ValueWrapper2 (t1 :: Symbol) a1 (t2 :: Symbol) a2 = ValueWrapper2
  { getValue1 :: a1,
    getValue2 :: a2
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (Hashable, NFData)

instance (KnownSymbol t1, KnownSymbol t2, HasCodec a1, HasCodec a2) => HasObjectCodec (ValueWrapper2 t1 a1 t2 a2) where
  objectCodec =
    ValueWrapper2
      <$> requiredField' (T.pack $ symbolVal (Proxy :: Proxy t1)) .= getValue1
      <*> requiredField' (T.pack $ symbolVal (Proxy :: Proxy t2)) .= getValue2

data ValueWrapper3 (t1 :: Symbol) a1 (t2 :: Symbol) a2 (t3 :: Symbol) a3 = ValueWrapper3
  { getValue1_ :: a1,
    getValue2_ :: a2,
    getValue3_ :: a3
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (Hashable, NFData)

instance
  (KnownSymbol t1, KnownSymbol t2, KnownSymbol t3, HasCodec a1, HasCodec a2, HasCodec a3) =>
  HasObjectCodec (ValueWrapper3 t1 a1 t2 a2 t3 a3)
  where
  objectCodec =
    ValueWrapper3
      <$> requiredField' (T.pack $ symbolVal (Proxy :: Proxy t1)) .= getValue1_
      <*> requiredField' (T.pack $ symbolVal (Proxy :: Proxy t2)) .= getValue2_
      <*> requiredField' (T.pack $ symbolVal (Proxy :: Proxy t3)) .= getValue3_
