{-# LANGUAGE TemplateHaskell #-}

-- | This module defines all backend-agnostic scalars we use throughout the
-- schema. This includes GraphQL scalars, and several other custom ones.
module Hasura.GraphQL.Parser.Internal.Scalars
  ( -- built-in types
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
  )
where

import Control.Monad ((>=>))
import Data.Aeson qualified as A
import Data.Aeson.Internal qualified as A.Internal
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

boolean :: MonadParse m => Parser origin 'Both m Bool
boolean = mkScalar GName._Boolean \case
  GraphQLValue (VBoolean b) -> pure b
  JSONValue (A.Bool b) -> pure b
  v -> typeMismatch GName._Boolean "a boolean" v

int :: MonadParse m => Parser origin 'Both m Int32
int = mkScalar GName._Int \case
  GraphQLValue (VInt i) -> scientificToInteger $ fromInteger i
  JSONValue (A.Number n) -> scientificToInteger n
  v -> typeMismatch GName._Int "a 32-bit integer" v

float :: MonadParse m => Parser origin 'Both m Double
float = mkScalar GName._Float \case
  GraphQLValue (VFloat f) -> scientificToFloat f
  GraphQLValue (VInt i) -> scientificToFloat $ fromInteger i
  JSONValue (A.Number n) -> scientificToFloat n
  v -> typeMismatch GName._Float "a float" v

string :: MonadParse m => Parser origin 'Both m Text
string = mkScalar GName._String \case
  GraphQLValue (VString s) -> pure s
  JSONValue (A.String s) -> pure s
  v -> typeMismatch GName._String "a string" v

-- | As an input type, any string or integer input value should be coerced to ID as Text
-- https://spec.graphql.org/June2018/#sec-ID
identifier :: MonadParse m => Parser origin 'Both m Text
identifier = mkScalar GName._ID \case
  GraphQLValue (VString s) -> pure s
  GraphQLValue (VInt i) -> pure . Text.pack $ show i
  JSONValue (A.String s) -> pure s
  JSONValue (A.Number n) -> parseScientific n
  v -> typeMismatch GName._ID "a String or a 32-bit integer" v
  where
    parseScientific = fmap (Text.pack . show @Int) . scientificToInteger

--------------------------------------------------------------------------------
-- Custom scalars

uuid :: MonadParse m => Parser origin 'Both m UUID.UUID
uuid = mkScalar name \case
  GraphQLValue (VString s) -> parseJSON $ A.String s
  JSONValue v -> parseJSON v
  v -> typeMismatch name "a UUID" v
  where
    name = $$(litName "uuid")

json, jsonb :: MonadParse m => Parser origin 'Both m A.Value
json = jsonScalar $$(litName "json") Nothing
jsonb = jsonScalar $$(litName "jsonb") Nothing

-- | Additional validation on integers. We do keep the same type name in the schema for backwards
-- compatibility.
-- TODO: when we can do a breaking change, we can rename the type to "NonNegativeInt".
nonNegativeInt :: MonadParse m => Parser origin 'Both m Int32
nonNegativeInt = mkScalar GName._Int \case
  GraphQLValue (VInt i) | i >= 0 -> scientificToInteger $ fromInteger i
  JSONValue (A.Number n) | n >= 0 -> scientificToInteger n
  v -> typeMismatch GName._Int "a non-negative 32-bit integer" v

-- | GraphQL ints are 32-bit integers; but in some places we want to accept bigger ints. To do so,
-- we declare a cusom scalar that can represent 64-bit ints, which accepts both int literals and
-- string literals. We do keep the same type name in the schema for backwards compatibility.
-- TODO: when we can do a breaking change, we can rename the type to "BigInt".
bigInt :: MonadParse m => Parser origin 'Both m Int64
bigInt = mkScalar GName._Int \case
  GraphQLValue (VInt i) -> scientificToInteger $ fromInteger i
  JSONValue (A.Number n) -> scientificToInteger n
  GraphQLValue (VString s)
    | Right (i, "") <- decimal s ->
      pure i
  JSONValue (A.String s)
    | Right (i, "") <- decimal s ->
      pure i
  v -> typeMismatch GName._Int "a 32-bit integer, or a 64-bit integer represented as a string" v

-- | Parser for 'Scientific'. Certain backends like BigQuery support
-- Decimal/BigDecimal and need an arbitrary precision number.
scientific :: MonadParse m => Parser origin 'Both m Scientific
scientific = mkScalar name \case
  GraphQLValue (VFloat f) -> pure f
  GraphQLValue (VInt i) -> pure $ S.scientific i 0
  JSONValue (A.Number n) -> pure n
  v -> typeMismatch name "Decimal represented as a string" v
  where
    name = $$(litName "decimal")

--------------------------------------------------------------------------------
-- Internal tools

-- | Creates a parser that transforms its input into a JSON value. 'valueToJSON'
-- does properly unpack variables.
jsonScalar :: MonadParse m => Name -> Maybe Description -> Parser origin 'Both m A.Value
jsonScalar name description =
  Parser
    { pType = schemaType,
      pParser = valueToJSON $ toGraphQLType schemaType
    }
  where
    schemaType = typeNamed name description

--------------------------------------------------------------------------------
-- Local helpers

mkScalar ::
  MonadParse m =>
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

parseJSON :: (MonadParse m, A.FromJSON b) => A.Value -> m b
parseJSON x =
  case A.Internal.iparse A.parseJSON x of
    A.Internal.IError path message -> withPath path $ parseErrorWith ParseFailed (toErrorMessage (Text.pack message))
    A.Internal.ISuccess result -> pure result
