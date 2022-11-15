module Autodocodec.Extended
  ( graphQLFieldNameCodec,
    graphQLValueCodec,
    hashSetCodec,
    hashSetCodecWith,
    integerCodec,
    optionalFieldOrIncludedNull,
    optionalFieldOrIncludedNull',
    optionalFieldOrIncludedNullWith,
    optionalFieldOrIncludedNullWith',
  )
where

import Autodocodec
import Data.HashSet qualified as HashSet
import Data.Scientific (Scientific (base10Exponent), floatingOrInteger)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Hasura.Metadata.DTO.Utils (typeableName)
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

-- | Codec for a GraphQL field name
graphQLFieldNameCodec :: JSONCodec G.Name
graphQLFieldNameCodec = bimapCodec dec enc codec
  where
    dec text =
      maybeToEither ("invalid GraphQL field name '" <> T.unpack text <> "'") $
        G.mkName text
    enc = G.unName

graphQLValueCodec :: forall var. Typeable var => JSONCodec var -> JSONCodec (G.Value var)
graphQLValueCodec varCodec =
  named ("GraphQLValue_" <> typeableName @var) $
    matchChoicesCodec
      [ (isVVariable, dimapCodec G.VVariable fromVVariable varCodec), -- The VVariable case must be first in case its codec overlaps with other cases
        (isVNull, dimapCodec (const G.VNull) (const ()) nullCodec),
        (isVInt, dimapCodec (G.VInt . toInteger) fromVInt integerCodec), -- It's important to try VInt first because the Scientific codec will match integers
        (isVFloat, dimapCodec G.VFloat fromVFloat codec),
        (isVString, dimapCodec G.VString fromVString codec),
        (isVBoolean, dimapCodec G.VBoolean fromVBoolean codec),
        (isVEnum, dimapCodec G.VEnum fromVEnum $ dimapCodec G.EnumValue G.unEnumValue graphQLFieldNameCodec),
        (isVList, dimapCodec G.VList fromVList $ listCodec (graphQLValueCodec varCodec)),
        (isVObject, dimapCodec G.VObject fromVObject $ hashMapCodec (graphQLValueCodec varCodec))
      ]
      unhandledCase
  where
    isVVariable = \case v@(G.VVariable _) -> Just v; _ -> Nothing
    isVNull = \case v@G.VNull -> Just v; _ -> Nothing
    isVInt = \case v@(G.VInt _) -> Just v; _ -> Nothing
    isVFloat = \case v@(G.VFloat _) -> Just v; _ -> Nothing
    isVString = \case v@(G.VString _) -> Just v; _ -> Nothing
    isVBoolean = \case v@(G.VBoolean _) -> Just v; _ -> Nothing
    isVEnum = \case v@(G.VEnum _) -> Just v; _ -> Nothing
    isVList = \case v@(G.VList _) -> Just v; _ -> Nothing
    isVObject = \case v@(G.VObject _) -> Just v; _ -> Nothing

    fromVVariable = \case (G.VVariable var) -> var; _ -> error "expected a VVariable"
    fromVInt = \case (G.VInt i) -> i; _ -> error "expected a VInt"
    fromVFloat = \case (G.VFloat f) -> f; _ -> error "expected a VFloat"
    fromVString = \case (G.VString s) -> s; _ -> error "expected a VString"
    fromVBoolean = \case (G.VBoolean b) -> b; _ -> error "expected a VBoolean"
    fromVEnum = \case (G.VEnum s) -> s; _ -> error "expected a VEnum"
    fromVList = \case (G.VList list) -> list; _ -> error "expected a VList"
    fromVObject = \case (G.VObject obj) -> obj; _ -> error "expected a VObject"

    unhandledCase =
      let msg = "no codec for value type"
          dec _ = Left msg -- handle failure without exception when decoding
          enc _ = error msg -- encoding is supposed to be total so we need an exception here
       in bimapCodec dec enc nullCodec

-- | Serializes a hash set by converting it to a list. This matches the FromJSON
-- and ToJSON instances in aeson.
hashSetCodec :: (Hashable a, HasCodec a) => JSONCodec (HashSet a)
hashSetCodec = hashSetCodecWith codec

-- | Serializes a hash set by converting it to a list. This matches the FromJSON
-- and ToJSON instances in aeson. This version accepts a codec for individual
-- set values as an argument.
hashSetCodecWith :: Hashable a => JSONCodec a -> JSONCodec (HashSet a)
hashSetCodecWith elemCodec =
  dimapCodec HashSet.fromList HashSet.toList $
    listCodec elemCodec

-- | Codec for integer with a generous bounds check that matches the behavior of
-- aeson integer deserialization.
integerCodec :: JSONCodec Integer
integerCodec = bimapCodec dec enc $ codec @Scientific
  where
    dec scientific =
      if exp10 > 1024
        then Left msg
        else parseIntegralFromScientific scientific
      where
        exp10 = base10Exponent scientific
        msg = "found a number with exponent " ++ show exp10 ++ ", but it must not be greater than 1024"
    enc = fromInteger

parseIntegralFromScientific :: (Integral a) => Scientific -> Either String a
parseIntegralFromScientific s = case floatingOrInteger @Float s of
  Right x -> Right x
  Left _ -> Left $ "unexpected floating number " <> show s

-- | An optional field that might be @null@ where a @Nothing@ value should be
-- represented as @null@ on serialization instead of omitting the field.
--
-- This differs from Autodocodec's stock 'optionalFieldOrNull' in that that
-- function omits the field during serialization if the Haskell value is
-- @Nothing@. This version includes the field with a serialized value of @null@.
optionalFieldOrIncludedNull ::
  HasCodec output =>
  -- | Key
  Text ->
  -- | Documentation
  Text ->
  ObjectCodec (Maybe output) (Maybe output)
optionalFieldOrIncludedNull key doc = optionalFieldOrIncludedNullWith key codec doc

-- | An optional field that might be @null@ where a @Nothing@ value should be
-- represented as @null@ on serialization instead of omitting the field.
--
-- This differs from Autodocodec's stock 'optionalFieldOrNull'' in that that
-- function omits the field during serialization if the Haskell value is
-- @Nothing@. This version includes the field with a serialized value of @null@.
optionalFieldOrIncludedNull' ::
  HasCodec output =>
  -- | Key
  Text ->
  ObjectCodec (Maybe output) (Maybe output)
optionalFieldOrIncludedNull' key = optionalFieldOrIncludedNullWith' key codec

-- | An optional field that might be @null@ where a @Nothing@ value should be
-- represented as @null@ on serialization instead of omitting the field.
--
-- This differs from Autodocodec's stock 'optionalFieldOrNullWith' in that that
-- function omits the field during serialization if the Haskell value is
-- @Nothing@. This version includes the field with a serialized value of @null@.
optionalFieldOrIncludedNullWith ::
  -- | Key
  Text ->
  -- | Codec for the value
  JSONCodec output ->
  -- | Documentation
  Text ->
  ObjectCodec (Maybe output) (Maybe output)
optionalFieldOrIncludedNullWith key c doc =
  orIncludedNullHelper $
    OptionalKeyCodec key (maybeCodec c) (Just doc)

-- | An optional field that might be @null@ where a @Nothing@ value should be
-- represented as @null@ on serialization instead of omitting the field.
--
-- This differs from Autodocodec's stock 'optionalFieldOrNullWith'' in that that
-- function omits the field during serialization if the Haskell value is
-- @Nothing@. This version includes the field with a serialized value of @null@.
optionalFieldOrIncludedNullWith' ::
  -- | Key
  Text ->
  -- | Codec for the value
  JSONCodec output ->
  ObjectCodec (Maybe output) (Maybe output)
optionalFieldOrIncludedNullWith' key c =
  orIncludedNullHelper $
    OptionalKeyCodec key (maybeCodec c) Nothing

orIncludedNullHelper :: ObjectCodec (Maybe (Maybe input)) (Maybe (Maybe output)) -> ObjectCodec (Maybe input) (Maybe output)
orIncludedNullHelper = dimapCodec dec enc
  where
    dec :: Maybe (Maybe input) -> Maybe input
    dec = \case
      Nothing -> Nothing
      Just Nothing -> Nothing
      Just (Just a) -> Just a
    enc :: Maybe output -> Maybe (Maybe output)
    enc = \case
      Nothing -> Just Nothing -- This is the case that differs from the stock `orNullHelper`
      Just a -> Just (Just a)
