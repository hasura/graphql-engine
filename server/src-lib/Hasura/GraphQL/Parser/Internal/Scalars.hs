-- | This module defines all backend-agnostic scalars we use throughout the
-- schema. This includes GraphQL scalars, and several other custom ones.

module Hasura.GraphQL.Parser.Internal.Scalars
  ( -- built-in types
    boolean
  , int
  , float
  , string
  , identifier

    -- custom extensions
  , uuid
  , json
  , jsonb
  , nonNegativeInt
  , bigInt

    -- internal
  , unsafeRawScalar
  , jsonScalar
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                  as A
import qualified Data.Aeson.Types                            as A
import qualified Data.UUID                                   as UUID

import           Data.Int                                    (Int32, Int64)
import           Data.Text.Read                              (decimal)
import           Language.GraphQL.Draft.Syntax               hiding (Definition)

import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.Base.Error
import           Hasura.GraphQL.Parser.Class.Parse
import           Hasura.GraphQL.Parser.Internal.Convert
import           Hasura.GraphQL.Parser.Internal.TypeChecking
import           Hasura.GraphQL.Parser.Internal.Types
import           Hasura.GraphQL.Parser.Schema
import           Hasura.RQL.Types.CustomTypes



--------------------------------------------------------------------------------
-- Built-in scalars

boolean :: MonadParse m => Parser 'Both m Bool
boolean = mkScalar boolScalar Nothing \case
  GraphQLValue (VBoolean b) -> pure b
  JSONValue    (A.Bool   b) -> pure b
  v                         -> typeMismatch boolScalar "a boolean" v

int :: MonadParse m => Parser 'Both m Int32
int = mkScalar intScalar Nothing \case
  GraphQLValue (VInt i)  -> convertWith scientificToInteger $ fromInteger i
  JSONValue (A.Number n) -> convertWith scientificToInteger n
  v                      -> typeMismatch intScalar "a 32-bit integer" v

float :: MonadParse m => Parser 'Both m Double
float = mkScalar floatScalar Nothing \case
  GraphQLValue (VFloat f)   -> convertWith scientificToFloat f
  GraphQLValue (VInt   i)   -> convertWith scientificToFloat $ fromInteger i
  JSONValue    (A.Number n) -> convertWith scientificToFloat n
  v                         -> typeMismatch floatScalar "a float" v

string :: MonadParse m => Parser 'Both m Text
string = mkScalar stringScalar Nothing \case
  GraphQLValue (VString  s) -> pure s
  JSONValue    (A.String s) -> pure s
  v                         -> typeMismatch stringScalar "a string" v

-- | As an input type, any string or integer input value should be coerced to ID as Text
-- https://spec.graphql.org/June2018/#sec-ID
identifier :: MonadParse m => Parser 'Both m Text
identifier = mkScalar idScalar Nothing \case
  GraphQLValue (VString  s) -> pure s
  GraphQLValue (VInt     i) -> pure $ tshow i
  JSONValue    (A.String s) -> pure s
  JSONValue    (A.Number n) -> parseScientific n
  v                         -> typeMismatch idScalar "a String or a 32-bit integer" v
  where
    parseScientific = convertWith $ fmap (tshow @Int) . scientificToInteger



--------------------------------------------------------------------------------
-- Custom scalars

uuid :: MonadParse m => Parser 'Both m UUID.UUID
uuid = mkScalar name Nothing \case
  GraphQLValue (VString s) -> convertWith A.parseJSON $ A.String s
  JSONValue    v           -> convertWith A.parseJSON v
  v                        -> typeMismatch name "a UUID" v
  where
    name = $$(litName "uuid")

json, jsonb :: MonadParse m => Parser 'Both m A.Value
json  = jsonScalar $$(litName "json")  Nothing
jsonb = jsonScalar $$(litName "jsonb") Nothing

-- | Additional validation on integers. We do keep the same type name in the schema for backwards
-- compatibility.
-- TODO: when we can do a breaking change, we can rename the type to "NonNegativeInt".
nonNegativeInt :: MonadParse m => Parser 'Both m Int32
nonNegativeInt = mkScalar intScalar Nothing \case
  GraphQLValue (VInt i)  | i >= 0 -> convertWith scientificToInteger $ fromInteger i
  JSONValue (A.Number n) | n >= 0 -> convertWith scientificToInteger n
  v                               -> typeMismatch intScalar "a non-negative 32-bit integer" v

-- | GraphQL ints are 32-bit integers; but in some places we want to accept bigger ints. To do so,
-- we declare a cusom scalar that can represent 64-bit ints, which accepts both int literals and
-- string literals. We do keep the same type name in the schema for backwards compatibility.
-- TODO: when we can do a breaking change, we can rename the type to "BigInt".
bigInt :: MonadParse m => Parser 'Both m Int64
bigInt = mkScalar intScalar Nothing \case
  GraphQLValue (VInt     i) -> convertWith scientificToInteger $ fromInteger i
  JSONValue    (A.Number n) -> convertWith scientificToInteger n
  GraphQLValue (VString  s)
    | Right (i, "") <- decimal s
    -> pure i
  JSONValue    (A.String s)
    | Right (i, "") <- decimal s
    -> pure i
  v -> typeMismatch intScalar "a 32-bit integer, or a 64-bit integer represented as a string" v



--------------------------------------------------------------------------------
-- Internal tools

-- | Explicitly define any desired scalar type.
--
-- This was considered unsafe because, unlike all other scalar definitions, it
-- doesn't enforce that we properly peel variables, and let the caller decide
-- how they want to deal with potential variables. This allows the caller to
-- bypass type-checking and (deprecated) non-reusability semantics (see comment
-- about re-usability in TypeChecking).
--
-- In practice, this function isn't very dangerous, and preferable to an
-- explicit use of the Parser constructor.
unsafeRawScalar
  :: MonadParse n
  => Name
  -> Maybe Description
  -> Parser 'Both n (InputValue Variable)
unsafeRawScalar name description = Parser
  { pType = NonNullable $ TNamed $ mkDefinition name description TIScalar
  , pParser = pure
  }

-- | Creates a parser that transforms its input into a JSON value. 'valueToJSON'
-- does properly unpack variables.
jsonScalar :: MonadParse m => Name -> Maybe Description -> Parser 'Both m A.Value
jsonScalar name description = Parser
  { pType = schemaType
  , pParser = valueToJSON $ toGraphQLType schemaType
  }
  where
    schemaType = NonNullable $ TNamed $ mkDefinition name description TIScalar



--------------------------------------------------------------------------------
-- Local helpers

mkScalar
  :: MonadParse m
  => Name
  -> Maybe Description
  -> (InputValue Variable -> m a)
  -> Parser 'Both m a
mkScalar name description parser = Parser
  { pType = schemaType
  , pParser = peelVariable (toGraphQLType schemaType) >=> parser
  }
  where
    schemaType = NonNullable $ TNamed $ mkDefinition name description TIScalar

convertWith
  :: MonadParse m
  => (a -> A.Parser b)
  -> (a -> m b)
convertWith f x = runAesonParser f x `onLeft` (parseErrorWith ParseFailed . qeError)
