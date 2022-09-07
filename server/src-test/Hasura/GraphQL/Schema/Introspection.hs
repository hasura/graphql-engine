{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This module contains functions to help with making assertions on the result
-- of parser introspection queries.
module Hasura.GraphQL.Schema.Introspection
  ( queryInputFieldsParserIntrospection,
  )
where

import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as AP
import Data.Aeson.Ordered qualified as AO
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.NonEmpty
import Hasura.Backends.Postgres.Instances.Schema ()
import Hasura.Backends.Postgres.SQL.DML (SQLExp (SELit))
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.SQL.Value (PGScalarValue (..))
import Hasura.Base.ErrorMessage
import Hasura.Base.ToErrorValue (ToErrorValue (toErrorValue))
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.GraphQL.Schema.BoolExp.AggregationPredicates
  ( ArgumentsSignature (..),
    FunctionSignature (..),
    defaultAggregationPredicatesParser,
  )
import Hasura.GraphQL.Schema.Introspect qualified as I
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (OpExpG (AEQ))
import Hasura.RQL.IR.BoolExp.AggregationPredicates
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Column (ColumnType (ColumnScalar), ColumnValue (..))
import Hasura.RQL.Types.Common (InsertOrder (..), RelName (..), RelType (..), SourceName (..))
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization (SourceCustomization (SourceCustomization))
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Vanilla))
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Test.Hspec
import Test.Hspec.Extended (dependentSpec)
import Test.Parser.Field qualified as GQL
import Test.Parser.Internal
import Test.Parser.Monad
import Type.Reflection (Typeable, typeRep)

-- | Produce an introspection parser for an 'InputFieldsParser'.
-- Use the "Test.Parser.Field.field" quasi-quoter to construct the introspection query.
queryInputFieldsParserIntrospection ::
  forall n a.
  -- | The Parser to introspect
  P.InputFieldsParser n a ->
  -- | The Introspection query
  G.Field G.NoFragments P.Variable ->
  IO A.Value
queryInputFieldsParserIntrospection parser field = do
  introspectionParser <- introspectDefintions (P.ifDefinitions parser)
  runParserTest $ P.fParser introspectionParser field

introspectDefintions ::
  forall n a.
  (P.HasTypeDefinitions a, P.MonadParse n) =>
  a ->
  IO (P.FieldParser n A.Value)
introspectDefintions definitions = do
  let introParser :: Either P.ConflictingDefinitions (P.FieldParser n A.Value) = do
        types <- P.collectTypeDefinitions [P.TypeDefinitionsWrapper definitions]
        let schema =
              P.Schema
                { sDescription = Nothing,
                  sTypes = types,
                  sQueryType =
                    P.TNamed
                      P.NonNullable
                      $ P.Definition GName._String Nothing Nothing [] (P.TIObject (P.ObjectInfo [] [])),
                  sMutationType = Nothing,
                  sSubscriptionType = Nothing,
                  sDirectives = []
                }
        return $ (AO.fromOrdered . ($ schema)) <$> I.schema @n

  onLeft introParser (error . T.unpack . fromErrorMessage . toErrorValue)
