-- | This module defines the top-level translation functions pertaining to
-- queries that are not aggregation queries, i.e. so-called "simple" selects
-- into Postgres AST.
module Hasura.Backends.Postgres.Translate.Select.Simple
  ( mkSQLSelect,
    selectQuerySQL,
  )
where

import Control.Monad.Writer.Strict (runWriter)
import Database.PG.Query (Query, fromBuilder)
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.IdentifierUniqueness (prefixNumToAliases)
import Hasura.Backends.Postgres.SQL.Types (IsIdentifier (toIdentifier))
import Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON
import Hasura.Backends.Postgres.Translate.Select.Internal.Extractor (asJsonAggExtr)
import Hasura.Backends.Postgres.Translate.Select.Internal.GenerateSelect (generateSQLSelectFromArrayNode)
import Hasura.Backends.Postgres.Translate.Select.Internal.Process (processAnnSimpleSelect)
import Hasura.Backends.Postgres.Translate.Types
import Hasura.Prelude
import Hasura.RQL.IR.Select
  ( AnnSelectG (_asnStrfyNum),
    AnnSimpleSelect,
  )
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.Common
  ( FieldName (FieldName),
    JsonAggSelect,
  )
import Hasura.SQL.Backend (BackendType (Postgres))
import Hasura.SQL.Types (ToSQL (toSQL))

-- | Translates IR to Postgres queries for simple SELECTs (select queries that
-- are not aggregations, including subscriptions).
--
-- See 'mkSQLSelect' for the Postgres AST.
selectQuerySQL ::
  forall pgKind.
  (Backend ('Postgres pgKind), PostgresAnnotatedFieldJSON pgKind) =>
  JsonAggSelect ->
  AnnSimpleSelect ('Postgres pgKind) ->
  Query
selectQuerySQL jsonAggSelect sel =
  fromBuilder $ toSQL $ mkSQLSelect jsonAggSelect sel

mkSQLSelect ::
  forall pgKind.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  JsonAggSelect ->
  AnnSimpleSelect ('Postgres pgKind) ->
  S.Select
mkSQLSelect jsonAggSelect annSel =
  let permLimitSubQuery = PLSQNotRequired
      -- run an intermediary step in order to obtain:
      -- SelectSource: the primary source, along with its where clause and sorting/slicing information
      -- NodeExtractors: a map from aliases which need to be selected to the corresponding required SQLExp to select the value
      -- JoinTree: the join tree required for relationships (built via @MonadWriter@)
      ((selectSource, nodeExtractors), joinTree) =
        runWriter $
          flip runReaderT strfyNum $
            processAnnSimpleSelect sourcePrefixes rootFldName permLimitSubQuery annSel
      selectNode = SelectNode nodeExtractors joinTree
      topExtractor =
        asJsonAggExtr jsonAggSelect rootFldAls permLimitSubQuery $
          orderByForJsonAgg selectSource
      arrayNode = MultiRowSelectNode [topExtractor] selectNode
   in prefixNumToAliases $
        generateSQLSelectFromArrayNode selectSource arrayNode $ S.BELit True
  where
    strfyNum = _asnStrfyNum annSel
    rootFldIdentifier = toIdentifier rootFldName
    sourcePrefixes = SourcePrefixes rootFldIdentifier rootFldIdentifier
    rootFldName = FieldName "root"
    rootFldAls = S.Alias $ toIdentifier rootFldName
