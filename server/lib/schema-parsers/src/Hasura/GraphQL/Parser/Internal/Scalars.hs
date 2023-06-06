{-# LANGUAGE TemplateHaskell #-}

-- | This module defines all backend-agnostic scalars we use throughout the
-- schema. This includes GraphQL scalars, and several other custom ones.
module Hasura.GraphQL.Parser.Internal.Scalars
  ( -- functions for constructing named scalar type parsers based on built-in types
    namedBoolean,
    namedInt,
    namedFloat,
    namedString,
    namedIdentifier,
    -- built-in types
    boolean,
    int,
    float,
    string,
    identifier,
    -- custom extensions
    uuid,
    json,
    jsonb,
    nonNegativeInt,
    bigInt,
    scientific,
    -- internal
    jsonScalar,
    mkScalar,
  )
where

import Control.Monad ((>=>))
import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J.Internal
import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import Data.Scientific qualified as S
import Data.Scientific qualified as Scientific
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read (decimal)
import Data.UUID qualified as UUID
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.ErrorCode
import Hasura.GraphQL.Parser.Internal.Convert
import Hasura.GraphQL.Parser.Internal.TypeChecking
import Hasura.GraphQL.Parser.Internal.Types
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.GraphQL.Parser.Schema
import Hasura.GraphQL.Parser.Variable
import Language.GraphQL.Draft.Syntax hiding (Definition)

-- Disable custom prelude warnings in preparation for extracting this module into a separate package.
{-# ANN module ("HLint: ignore Use onLeft" :: String) #-}

{-# ANN module ("HLint: ignore Use onNothing" :: String) #-}

{-# ANN module ("HLint: ignore Use tshow" :: String) #-}

--------------------------------------------------------------------------------
-- Built-in scalars

namedBoolean :: (MonadParse m) => Name -> Parser origin 'Both m Bool
namedBoolean name = mkScalar name \case
  GraphQLValue (VBoolean b) -> pure b
  JSONValue (J.Bool b) -> pure b
  v -> typeMismatch name "a boolean" v

boolean :: (MonadParse m) => Parser origin 'Both m Bool
boolean = namedBoolean GName._Boolean

namedInt :: (MonadParse m) => Name -> Parser origin 'Both m Int32
namedInt name = mkScalar name \case
  GraphQLValue (VInt i) -> scientificToInteger $ fromInteger i
  JSONValue (J.Number n) -> scientificToInteger n
  v -> typeMismatch name "a 32-bit integer" v

int :: (MonadParse m) => Parser origin 'Both m Int32
int = namedInt GName._Int

namedFloat :: (MonadParse m) => Name -> Parser origin 'Both m Double
namedFloat name = mkScalar name \case
  GraphQLValue (VFloat f) -> scientificToFloat f
  GraphQLValue (VInt i) -> scientificToFloat $ fromInteger i
  JSONValue (J.Number n) -> scientificToFloat n
  v -> typeMismatch name "a float" v

float :: (MonadParse m) => Parser origin 'Both m Double
float = namedFloat GName._Float

namedString :: (MonadParse m) => Name -> Parser origin 'Both m Text
namedString name = mkScalar name \case
  GraphQLValue (VString s) -> pure s
  JSONValue (J.String s) -> pure s
  v -> typeMismatch name "a string" v

string :: (MonadParse m) => Parser origin 'Both m Text
string = namedString GName._String

-- | As an input type, any string or integer input value should be coerced to ID as Text
-- https://spec.graphql.org/June2018/#sec-ID
namedIdentifier :: (MonadParse m) => Name -> Parser origin 'Both m Text
namedIdentifier name = mkScalar name \case
  GraphQLValue (VString s) -> pure s
  GraphQLValue (VInt i) -> pure . Text.pack $ show i
  JSONValue (J.String s) -> pure s
  JSONValue (J.Number n) -> parseScientific n
  v -> typeMismatch name "a String or a 32-bit integer" v
  where
    parseScientific = fmap (Text.pack . show @Int) . scientificToInteger

identifier :: (MonadParse m) => Parser origin 'Both m Text
identifier = namedIdentifier GName._ID

--------------------------------------------------------------------------------
-- Custom scalars

uuid :: (MonadParse m) => Parser origin 'Both m UUID.UUID
uuid = mkScalar name \case
  GraphQLValue (VString s) -> parseJSON $ J.String s
  JSONValue v -> parseJSON v
  v -> typeMismatch name "a UUID" v
  where
    name = $$(litName "uuid")

json, jsonb :: (MonadParse m) => Parser origin 'Both m J.Value
json = jsonScalar $$(litName "json") Nothing
jsonb = jsonScalar $$(litName "jsonb") Nothing

-- | Additional validation on integers. We do keep the same type name in the schema for backwards
-- compatibility.
-- TODO: when we can do a breaking change, we can rename the type to "NonNegativeInt".
nonNegativeInt :: (MonadParse m) => Parser origin 'Both m Int32
nonNegativeInt = mkScalar GName._Int \case
  GraphQLValue (VInt i) | i >= 0 -> scientificToInteger $ fromInteger i
  JSONValue (J.Number n) | n >= 0 -> scientificToInteger n
  v -> typeMismatch GName._Int "a non-negative 32-bit integer" v

-- | GraphQL ints are 32-bit integers; but in some places we want to accept bigger ints. To do so,
-- we declare a cusom scalar that can represent 64-bit ints, which accepts both int literals and
-- string literals. We do keep the same type name in the schema for backwards compatibility.
-- TODO: when we can do a breaking change, we can rename the type to "BigInt".
bigInt :: (MonadParse m) => Parser origin 'Both m Int64
bigInt = mkScalar GName._Int \case
  GraphQLValue (VInt i) -> scientificToInteger $ fromInteger i
  JSONValue (J.Number n) -> scientificToInteger n
  GraphQLValue (VString s)
    | Right (i, "") <- decimal s ->
        pure i
  JSONValue (J.String s)
    | Right (i, "") <- decimal s ->
        pure i
  v -> typeMismatch GName._Int "a 32-bit integer, or a 64-bit integer represented as a string" v

-- | Parser for 'Scientific'. Certain backends like BigQuery support
-- Decimal/BigDecimal and need an arbitrary precision number.
scientific :: (MonadParse m) => Parser origin 'Both m Scientific
scientific = mkScalar name \case
  GraphQLValue (VFloat f) -> pure f
  GraphQLValue (VInt i) -> pure $ S.scientific i 0
  JSONValue (J.Number n) -> pure n
  v -> typeMismatch name "Decimal represented as a string" v
  where
    name = $$(litName "decimal")

--------------------------------------------------------------------------------
-- Internal tools

-- | Creates a parser that transforms its input into a JSON value. 'valueToJSON'
-- does properly unpack variables.
jsonScalar :: (MonadParse m) => Name -> Maybe Description -> Parser origin 'Both m J.Value
jsonScalar name description =
  Parser
    { pType = schemaType,
      pParser = valueToJSON $ toGraphQLType schemaType
    }
  where
    schemaType = typeNamed name description

--------------------------------------------------------------------------------
-- Local helpers

-- | Creates a custom scalar, exposed in the schema with the given name.
mkScalar ::
  (MonadParse m) =>
  Name ->
  (InputValue Variable -> m a) ->
  Parser origin 'Both m a
mkScalar name parser =
  Parser
    { pType = schemaType,
      pParser = peelVariable (toGraphQLType schemaType) >=> parser
    }
  where
    schemaType = typeNamed name Nothing

typeNamed :: Name -> Maybe Description -> Type origin 'Both
typeNamed name description = TNamed NonNullable $ Definition name description Nothing [] TIScalar

scientificToInteger :: (MonadParse m, Integral i, Bounded i) => Scientific -> m i
scientificToInteger num =
  maybe (parseErrorWith ParseFailed failure) pure $ Scientific.toBoundedInteger num
  where
    failure = "The value " <> toErrorMessage (Text.pack (show num)) <> " lies outside the bounds or is not an integer. Maybe it is a float, or is there integer overflow?"

scientificToFloat :: (MonadParse m, RealFloat f) => Scientific -> m f
scientificToFloat num =
  either (const (parseErrorWith ParseFailed failure)) pure $ Scientific.toBoundedRealFloat num
  where
    failure = "The value " <> toErrorMessage (Text.pack (show num)) <> " lies outside the bounds. Is it overflowing the float bounds?"

parseJSON :: (MonadParse m, J.FromJSON b) => J.Value -> m b
parseJSON x =
  case J.Internal.iparse J.parseJSON x of
    J.Internal.IError path message -> withPath path $ parseErrorWith ParseFailed (toErrorMessage (Text.pack message))
    J.Internal.ISuccess result -> pure result
