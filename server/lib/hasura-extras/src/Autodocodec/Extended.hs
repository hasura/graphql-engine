{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Autodocodec.Extended
  ( baseUrlCodec,
    caseInsensitiveHashMapCodec,
    caseInsensitiveTextCodec,
    graphQLEnumValueCodec,
    graphQLExecutableDocumentCodec,
    graphQLFieldDescriptionCodec,
    graphQLFieldNameCodec,
    graphQLValueCodec,
    graphQLSchemaDocumentCodec,
    hashSetCodec,
    hashSetCodecWith,
    integralWithBoundsCodec,
    integralWithLowerBoundCodec,
    integralWithUpperBoundCodec,
    integerCodec,
    optionalFieldOrIncludedNull,
    optionalFieldOrIncludedNull',
    optionalFieldOrIncludedNullWith,
    optionalFieldOrIncludedNullWith',
    realFracWithBoundsCodec,
    realFracWithLowerBoundCodec,
    realFracWithUpperBoundCodec,
    refinedCodec,
    refinedCodecWith,
    typeableName,
    unitCodec,
    DisjunctCodec (..),
    disjointMatchChoicesNECodec,
    boolConstCodec,
    boundedEnumCodec,
    discriminatorField,
    discriminatorBoolField,
    fromEnvCodec,
    optionalVersionField,
    versionField,
    oneOrManyCodec,
    module Autodocodec,
  )
where

import Autodocodec
import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.CaseInsensitive qualified as CI
import Data.Char (isAlphaNum)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Scientific (Scientific (base10Exponent), floatingOrInteger)
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import Hasura.Prelude
import Language.GraphQL.Draft.Parser qualified as G
import Language.GraphQL.Draft.Parser qualified as GParser
import Language.GraphQL.Draft.Printer qualified as G
import Language.GraphQL.Draft.Printer qualified as GPrinter
import Language.GraphQL.Draft.Syntax qualified as G
import Refined qualified as R
import Servant.Client (BaseUrl)
import Servant.Client qualified as S
import Text.Builder qualified as TB

instance HasCodec () where
  codec = unitCodec

-- | Codec for URL type from the servant-client package.
baseUrlCodec :: JSONCodec BaseUrl
baseUrlCodec = bimapCodec dec enc stringCodec
  where
    dec t = case S.parseBaseUrl t of
      Just u -> Right u
      Nothing -> Left $ "Invalid base url: " ++ t
    enc = S.showBaseUrl

-- | Like 'hashMapCodec', but with case-insensitive keys.
caseInsensitiveHashMapCodec ::
  forall k a.
  (CI.FoldCase k, Hashable k, FromJSONKey k, ToJSONKey k) =>
  JSONCodec a ->
  JSONCodec (HashMap.HashMap (CI.CI k) a)
caseInsensitiveHashMapCodec elemCodec =
  dimapCodec
    (mapKeys CI.mk)
    (mapKeys CI.original)
    $ hashMapCodec elemCodec

-- | Codec for case-insensitive strings / text. The underlying value may be
-- @Text@ or another type that implements @FoldCase@ and @HasCodec@.
caseInsensitiveTextCodec :: forall a. (CI.FoldCase a, HasCodec a) => JSONCodec (CI.CI a)
caseInsensitiveTextCodec = dimapCodec CI.mk CI.original codec

graphQLEnumValueCodec :: JSONCodec G.EnumValue
graphQLEnumValueCodec = dimapCodec G.EnumValue G.unEnumValue graphQLFieldNameCodec

graphQLExecutableDocumentCodec :: JSONCodec (G.ExecutableDocument G.Name)
graphQLExecutableDocumentCodec = bimapCodec dec enc codec
  where
    dec = mapLeft T.unpack . G.parseExecutableDoc
    enc = G.renderExecutableDoc

-- | Codec for a GraphQL field name
graphQLFieldNameCodec :: JSONCodec G.Name
graphQLFieldNameCodec = named "GraphQLName" $ bimapCodec dec enc codec
  where
    dec text =
      maybeToEither ("invalid GraphQL field name '" <> T.unpack text <> "'")
        $ G.mkName text
    enc = G.unName

graphQLFieldDescriptionCodec :: JSONCodec G.Description
graphQLFieldDescriptionCodec = dimapCodec G.Description G.unDescription codec

graphQLValueCodec :: forall var. (Typeable var) => JSONCodec var -> JSONCodec (G.Value var)
graphQLValueCodec varCodec =
  named ("GraphQLValue_" <> typeableName @var)
    $ matchChoicesCodec
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

graphQLSchemaDocumentCodec :: JSONCodec G.SchemaDocument
graphQLSchemaDocumentCodec = named "GraphQLSchema" $ bimapCodec dec enc $ codec @Text
  where
    dec = mapLeft T.unpack . GParser.parseSchemaDocument
    enc = TB.run . GPrinter.schemaDocument

-- | Serializes a hash set by converting it to a list. This matches the FromJSON
-- and ToJSON instances in aeson.
hashSetCodec :: (Hashable a, HasCodec a) => JSONCodec (HashSet a)
hashSetCodec = hashSetCodecWith codec

-- | Serializes a hash set by converting it to a list. This matches the FromJSON
-- and ToJSON instances in aeson. This version accepts a codec for individual
-- set values as an argument.
hashSetCodecWith :: (Hashable a) => JSONCodec a -> JSONCodec (HashSet a)
hashSetCodecWith elemCodec =
  dimapCodec HashSet.fromList HashSet.toList
    $ listCodec elemCodec

-- | Codec for integral numbers with specified lower and upper bounds.
integralWithBoundsCodec :: (Integral i, Bounded i) => NumberBounds -> JSONCodec i
integralWithBoundsCodec bounds =
  bimapCodec go fromIntegral $ scientificWithBoundsCodec bounds
  where
    go s = case Scientific.toBoundedInteger s of
      Nothing -> Left $ "Number did not fit into given bounds: " <> show s
      Just i -> Right i

-- | Codec for integral numbers with specified lower bound.
integralWithLowerBoundCodec :: forall i. (Integral i, Bounded i) => i -> JSONCodec i
integralWithLowerBoundCodec minInt =
  integralWithBoundsCodec
    $ NumberBounds (fromIntegral minInt) (fromIntegral (maxBound @i))

-- | Codec for integral numbers with specified lower bound.
integralWithUpperBoundCodec :: forall i. (Integral i, Bounded i) => i -> JSONCodec i
integralWithUpperBoundCodec maxInt =
  integralWithBoundsCodec
    $ NumberBounds (fromIntegral (minBound @i)) (fromIntegral maxInt)

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
  (HasCodec output) =>
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
  (HasCodec output) =>
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
  orIncludedNullHelper
    $ OptionalKeyCodec key (maybeCodec c) (Just doc)

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
  orIncludedNullHelper
    $ OptionalKeyCodec key (maybeCodec c) Nothing

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

-- | Codec for fractional numeric type with specified lower and upper bounds.
realFracWithBoundsCodec :: (Real r, Fractional r) => NumberBounds -> JSONCodec r
realFracWithBoundsCodec bounds =
  dimapCodec realToFrac realToFrac $ scientificWithBoundsCodec bounds

-- | Codec for fractional numeric type with specified lower bound.
realFracWithLowerBoundCodec :: forall r. (Real r, Fractional r) => Scientific -> JSONCodec r
realFracWithLowerBoundCodec minReal = realFracWithBoundsCodec @r $ NumberBounds minReal infinity
  where
    infinity = realToFrac (1 / 0 :: Double)

-- | Codec for fractional numeric type with specified upper bound.
realFracWithUpperBoundCodec :: forall r. (Real r, Fractional r) => Scientific -> JSONCodec r
realFracWithUpperBoundCodec maxReal = realFracWithBoundsCodec @r $ NumberBounds negInfinity maxReal
  where
    negInfinity = realToFrac (-1 / 0 :: Double)

-- | Codec for values wrapped with a type-level predicate using the refined
-- package.
--
-- This version assumes that the underlying value type implements @HasCodec@.
refinedCodec :: (HasCodec a, R.Predicate p a) => JSONCodec (R.Refined p a)
refinedCodec = refinedCodecWith codec

-- | Codec for values wrapped with a type-level predicate using the refined
-- package.
--
-- This version requires a codec to be provided for the underlying value type.
refinedCodecWith :: (R.Predicate p a) => JSONCodec a -> JSONCodec (R.Refined p a)
refinedCodecWith underlyingCodec = bimapCodec dec enc underlyingCodec
  where
    dec = mapLeft show . R.refine
    enc = R.unrefine

-- | Provides a string based on the given type to use to uniquely name
-- instantiations of polymorphic codecs.
typeableName :: forall a. (Typeable a) => Text
typeableName = T.map toValidChar $ tshow $ typeRep (Proxy @a)
  where
    toValidChar c = if isAlphaNum c then c else '_'

-- | Serializes () the same way that the stock Aeson instance does
unitCodec :: JSONCodec ()
unitCodec = dimapCodec (const ()) (const []) (listCodec nullCodec)

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

-- | Map a fixed set of two values to boolean values when serializing. The first
-- argument is the value to map to @True@, the second is the value to map to
-- @False@.
boolConstCodec :: (Eq a) => a -> a -> JSONCodec a
boolConstCodec trueCase falseCase =
  dimapCodec
    (bool trueCase falseCase)
    (== trueCase)
    $ codec @Bool

-- | A codec for a 'Bounded' 'Enum' that maps to literal strings using
-- a provided function.
--
--
-- === Example usage
--
-- >>> data Fruit = FruitApple | FruitOrange deriving (Show, Eq, Enum, Bounded)
-- >>> let c = boundedEnumCodec (snakeCase . drop 5)
-- >>> toJSONVia c Apple
-- String "apple"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "orange") :: Maybe Fruit
-- Just Orange
boundedEnumCodec ::
  forall enum.
  (Eq enum, Enum enum, Bounded enum) =>
  (enum -> String) ->
  JSONCodec enum
boundedEnumCodec display =
  let ls = [minBound .. maxBound]
   in case NE.nonEmpty ls of
        Nothing -> error "0 enum values ?!"
        Just ne -> stringConstCodec (NE.map (\v -> (v, T.pack (display v))) ne)

-- | Defines a required object field named @version@ that must have the given
-- integer value. On serialization the field will have the given value
-- automatically. On deserialization parsing will fail unless the field has the
-- exact given value.
versionField :: Integer -> ObjectCodec a Scientific
versionField v = requiredFieldWith' "version" (EqCodec n scientificCodec) .= const n
  where
    n = fromInteger v

-- | Defines an optional object field named @version@ that must have the given
-- integer value if the field is present. On serialization the field will have
-- the given value automatically. On deserialization parsing will fail unless
-- the field has the exact given value, or is absent.
optionalVersionField :: Integer -> ObjectCodec a (Maybe Scientific)
optionalVersionField v =
  optionalFieldWith' "version" (EqCodec n scientificCodec) .= const (Just n)
  where
    n = fromInteger v

-- | Useful in an object codec for a field that indicates the type of the
-- object within a union. This version assumes that the type of the
-- discriminator field is @Text@.
discriminatorField :: Text -> Text -> ObjectCodec a ()
discriminatorField name value =
  dimapCodec (const ()) (const value)
    $ requiredFieldWith' name (literalTextCodec value)

-- | Useful in an object codec for a field that indicates the type of the
-- object within a union. This version assumes that the type of the
-- discriminator field is @Bool@.
discriminatorBoolField :: Text -> Bool -> ObjectCodec a ()
discriminatorBoolField name value =
  dimapCodec (const ()) (const value)
    $ requiredFieldWith' name (EqCodec value boolCodec)

-- | Represents a text field wrapped in an object with a single property
-- named @from_env@.
--
-- Objects of this form appear in many places in the Metadata API. If we
-- reproduced this codec in each use case the OpenAPI document would have many
-- identical object definitions. Using a shared codec allows a single shared
-- reference.
fromEnvCodec :: JSONCodec Text
fromEnvCodec = object "FromEnv" $ requiredField' "from_env"

oneOrManyCodec :: forall a. (HasCodec a) => JSONCodec [a]
oneOrManyCodec =
  matchChoiceCodec singletonCodec (codec @[a]) chooser
  where
    singletonCodec = dimapCodec pure id (codec @a)
    chooser = \case
      [x] -> Left x
      xs -> Right xs
