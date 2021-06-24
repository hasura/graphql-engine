{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.SQLite.Instances.Schema () where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax               as G

import           Data.Text.Extended

import qualified Hasura.GraphQL.Parser                       as P
import qualified Hasura.SQL.AnyBackend                       as AB

import           Hasura.Backends.Postgres.SQL.Types          (pgFmtLit)
import           Hasura.Base.Error
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Parser.Internal.Parser
import           Hasura.GraphQL.Parser.Internal.TypeChecking
import           Hasura.GraphQL.Schema.Backend
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.RQL.IR
import           Hasura.RQL.Types


--------------------------------------------------------------------------------
-- BackendSchema instance

instance BackendSchema 'SQLite where
  -- top level parsers
  buildTableQueryFields          = buildTableQueryField
  buildTableRelayQueryFields     = \_ _ _ _ _ _ _   -> pure []
  buildTableInsertMutationFields = \_ _ _ _ _ _ _ _ -> pure []
  buildTableUpdateMutationFields = \_ _ _ _ _ _ _   -> pure []
  buildTableDeleteMutationFields = \_ _ _ _ _ _ _   -> pure []
  buildFunctionQueryFields       = \_ _ _ _ _ _     -> pure []
  buildFunctionRelayQueryFields  = \_ _ _ _ _ _ _   -> pure []
  buildFunctionMutationFields    = \_ _ _ _ _ _     -> pure []

  -- backend extensions
  relayExtension    = Nothing
  nodesAggExtension = Nothing

  -- table arguments
  tableArguments = slTableArgs

  -- individual components
  columnParser              = slColumnParser
  jsonPathArg               = \_ -> pure Nothing
  comparisonExps            = slComparisonExps
  orderByOperators          = undefined
  updateOperators           = undefined
  mkCountType               = undefined
  aggregateOrderByCountType = undefined
  computedField             = undefined
  node                      = undefined

  -- SQL literals
  columnDefaultValue = undefined


--------------------------------------------------------------------------------
-- Top level parsers

buildTableQueryField
  :: forall r m n
   . MonadBuildSchema 'SQLite r m n
  => SourceName
  -> SourceConfig 'SQLite
  -> TableName 'SQLite
  -> TableInfo 'SQLite
  -> G.Name
  -> SelPermInfo 'SQLite
  -> m [FieldParser n (QueryRootField UnpreparedValue UnpreparedValue)]
buildTableQueryField sourceName sourceInfo tableName _tableInfo _selectName _selPerms =
   turnIntoRootField <$> undefined
  where
    _selectDesc :: Maybe G.Description
    _selectDesc = Just $ G.Description $ "fetch data from the table: " <>> tableName
    turnIntoRootField :: FieldParser n (SelectExp 'SQLite) -> [FieldParser n (QueryRootField UnpreparedValue UnpreparedValue)]
    turnIntoRootField =
      pure . fmap (RFDB sourceName . AB.mkAnyBackend . SourceConfigWith sourceInfo . QDBR . QDBMultipleRows)


--------------------------------------------------------------------------------
-- Table arguments

slTableArgs
  :: forall r m n
   . MonadBuildSchema 'SQLite r m n
  => SourceName
  -> TableInfo 'SQLite
  -> SelPermInfo 'SQLite
  -> m (InputFieldsParser n (SelectArgsG 'SQLite (UnpreparedValue 'SQLite)))
slTableArgs _sourceName _tableInfo _selectPermissions = do
  -- get parsers
  pure do
    -- use parsers
    pure $ SelectArgs
      { _saWhere    = undefined
      , _saOrderBy  = undefined
      , _saLimit    = undefined
      , _saOffset   = undefined
      , _saDistinct = undefined
      }



--------------------------------------------------------------------------------
-- Individual components

slColumnParser
  :: (MonadSchema n m)
  => ColumnType 'SQLite
  -> G.Nullability
  -> m (Parser 'Both n (Opaque (ColumnValue 'SQLite)))
slColumnParser columnType (G.Nullability isNullable) =
  opaque . fmap (ColumnValue columnType) <$> case columnType of
    ColumnScalar _        -> pure $ possiblyNullable P.string
    ColumnEnumReference _ -> error "enum not supported yet"
  where
    opaque :: MonadParse m => Parser 'Both m a -> Parser 'Both m (Opaque a)
    opaque parser = parser
      { pParser = \case
          P.GraphQLValue (G.VVariable var@Variable{ vInfo, vValue }) -> do
            typeCheck False (P.toGraphQLType $ pType parser) var
            P.mkOpaque (Just vInfo) <$> pParser parser (absurd <$> vValue)
          value -> P.mkOpaque Nothing <$> pParser parser value
      }
    possiblyNullable
      | isNullable = fmap (fromMaybe "null") . P.nullable
      | otherwise  = id

slComparisonExps
  :: forall m n
   . ( BackendSchema 'SQLite
     , MonadSchema n m
     , MonadError QErr m
     )
  => ColumnType 'SQLite
  -> m (Parser 'Input n [ComparisonExp 'SQLite])
slComparisonExps = P.memoize 'comparisonExps \columnType -> do
  typedParser <- columnParser columnType (G.Nullability False)
  let
    columnListParser = P.list typedParser `P.bind` traverse P.openOpaque
    name = P.getName typedParser <> $$(G.litName "_sqlite_comparison_exp")
    desc = G.Description $ "Boolean expression to compare columns of type "
      <>  P.getName typedParser
      <<> ". All fields are combined with logical 'AND'."
  pure $ P.object name (Just desc) $ fmap catMaybes $ sequenceA $ concat
    [ equalityOperators   False (mkParameter <$> typedParser) (mkListLiteral <$> columnListParser)
    , comparisonOperators False (mkParameter <$> typedParser)
    ]
  where
    mkListLiteral vals = P.UVLiteral $ "(" <> commaSeparated (pgFmtLit . cvValue <$> vals) <> ")"
