{-# LANGUAGE QuasiQuotes #-}

-- | This module defines the scalars we use specific to the BigQuery
-- schema.
--
-- An idiosyncracy of BigQuery is that numbers serialized via JSON uses string
-- literals instead of number literals, because BigQuery handles wider-bit
-- numbers than JSON/JavaScript does.
--
-- Therefore, the BigQuery Backend uses bespoke parsers for numeric scalar
-- input, which accept string literals as well as number literals, such that we
-- preserve symmetry with with output formats.
module Hasura.Backends.BigQuery.Parser.Scalars
  ( bqInt64,
    bqFloat64,
    bqDecimal,
    bqBigDecimal,
  )
where

import Data.Aeson qualified as J
import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Scientific qualified as S
import Data.Scientific qualified as Scientific
import Data.Text qualified as Text
import Hasura.Backends.BigQuery.Types qualified as BigQuery
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.Base.ErrorValue (dquote)
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.ErrorCode
import Hasura.GraphQL.Parser.Internal.TypeChecking
import Hasura.GraphQL.Parser.Internal.Types
import Hasura.GraphQL.Parser.Schema
import Hasura.GraphQL.Parser.Variable
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax hiding (Definition)
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Text.ParserCombinators.ReadP

bqInt64 :: forall origin m. (MonadParse m) => Parser origin 'Both m BigQuery.Int64
bqInt64 = mkScalar name "64-bit integers. Accepts both string and number literals." \case
  GraphQLValue (VInt i)
    | checkIntegerBounds i -> return $ BigQuery.Int64 (tshow i)
    | otherwise -> boundsFailure (tshow i)
  GraphQLValue (VString s) -> integralText s
  JSONValue (J.String s) -> integralText s
  JSONValue (J.Number n) -> integralSci (tshow n) n
  v -> typeMismatch name "a 64-bit integer" v
  where
    name = [G.name|bigquery_int|]

    checkIntegerBounds :: Integer -> Bool
    checkIntegerBounds i = toInteger (minBound @Int64) <= i && i <= toInteger (maxBound @Int64)

    integralText :: Text -> m BigQuery.Int64
    integralText inputText
      | [(sci, "")] <- readP_to_S Scientific.scientificP (Text.unpack inputText) = integralSci inputText sci
      | otherwise = stringNotationError name inputText

    integralSci :: Text -> Scientific -> m BigQuery.Int64
    integralSci inputText sci
      | Scientific.isInteger sci =
          case Scientific.toBoundedInteger @Int64 sci of
            Just v -> return $ BigQuery.intToInt64 v
            Nothing -> boundsFailure inputText
      | otherwise = integralFailure inputText

    boundsFailure, integralFailure :: forall a. Text -> m a
    boundsFailure inputText = parseErrorWith ParseFailed $ "The value " <> toErrorMessage inputText <> " lies outside the accepted numerical integral bounds."
    integralFailure inputText = parseErrorWith ParseFailed $ "The value " <> toErrorMessage inputText <> " has a non-zero fractional part."

bqFloat64 :: forall origin m. (MonadParse m) => Parser origin 'Both m BigQuery.Float64
bqFloat64 = mkScalar name "64-bit floats. Accepts both string and number literals." \case
  GraphQLValue (VFloat f) -> floatSci (tshow f) f
  GraphQLValue (VInt i) -> floatSci (tshow i) (fromInteger i)
  GraphQLValue (VString s) -> floatText s
  JSONValue (J.String s) -> floatText s
  JSONValue (J.Number n) -> floatSci (tshow n) n
  v -> typeMismatch name "a 64-bit float" v
  where
    name = [G.name|bigquery_float|]

    floatText :: Text -> m BigQuery.Float64
    floatText inputText
      | [(sci, "")] <- readP_to_S Scientific.scientificP (Text.unpack inputText) = floatSci inputText sci
      | otherwise = stringNotationError name inputText

    floatSci :: Text -> Scientific -> m BigQuery.Float64
    floatSci inputText sci =
      case Scientific.toBoundedRealFloat @Double sci of
        Right v -> return $ BigQuery.doubleToFloat64 v
        Left _ -> boundsFailure inputText

    boundsFailure :: forall a. Text -> m a
    boundsFailure inputText = parseErrorWith ParseFailed $ "The value " <> toErrorMessage inputText <> " lies outside the accepted numerical integral bounds."

bqBigDecimal :: (MonadParse m) => Parser origin 'Both m BigQuery.BigDecimal
bqBigDecimal = mkScalar name "BigDecimals. Accepts both string and number literals." $ fmap (BigQuery.BigDecimal . BigQuery.scientificToText) . decimal name
  where
    name = [G.name|bigquery_bigdecimal|]

bqDecimal :: (MonadParse m) => Parser origin 'Both m BigQuery.Decimal
bqDecimal = mkScalar name "Decimals. Accepts both string and number literals." $ fmap (BigQuery.Decimal . BigQuery.scientificToText) . decimal name
  where
    name = [G.name|bigquery_decimal|]

decimal :: (MonadParse f) => Name -> InputValue Variable -> f Scientific
decimal name = \case
  GraphQLValue (VFloat f) -> pure f
  GraphQLValue (VInt i) -> pure $ S.scientific i 0
  GraphQLValue (VString s)
    | Just sci <- readMaybe (Text.unpack s) -> pure $ sci
    | otherwise -> stringNotationError name s
  JSONValue (J.Number n) -> pure n
  JSONValue (J.String s)
    | Just sci <- readMaybe (Text.unpack s) -> pure $ sci
    | otherwise -> stringNotationError name s
  v -> typeMismatch name "decimal" v

--------------------------------------------------------------------------------
-- Local helpers

mkScalar ::
  (MonadParse m) =>
  Name ->
  Description ->
  (InputValue Variable -> m a) ->
  Parser origin 'Both m a
mkScalar name desc parser =
  Parser
    { pType = schemaType,
      pParser = peelVariable (toGraphQLType schemaType) >=> parser
    }
  where
    schemaType = typeNamed name (Just desc)

typeNamed :: Name -> Maybe Description -> Type origin 'Both
typeNamed name description = TNamed NonNullable $ Definition name description Nothing [] TIScalar

stringNotationError :: (MonadParse m) => G.Name -> Text -> m a
stringNotationError typeName actualString =
  parseError
    $ "expected "
    <> toErrorMessage (tshow typeName)
    <> " represented as a string, but got "
    <> dquote actualString
    <> ", which is not a recognizable "
    <> toErrorMessage (tshow typeName)
    <> "."
