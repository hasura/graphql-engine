-- | This module defines the top-level translation functions pertaining to
-- queries that use aggregation (i.e., /not/ so-called "simple" selects) into
-- Postgres AST.
module Hasura.Backends.Postgres.Translate.Select.Aggregate
  ( mkAggregateSelect,
    selectAggregateQuerySQL,
  )
where

import Control.Monad.Writer.Strict (runWriter)
import Database.PG.Query (Query)
import Hasura.Backends.Postgres.SQL.DML (BoolExp (BELit), Select)
import Hasura.Backends.Postgres.SQL.Types (IsIdentifier (toIdentifier))
import Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON
import Hasura.Backends.Postgres.Translate.Select.Internal.GenerateSelect (generateSQLSelectFromArrayNode)
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers (selectToSelectWith, toQuery)
import Hasura.Backends.Postgres.Translate.Select.Internal.Process (processAnnAggregateSelect)
import Hasura.Backends.Postgres.Translate.Types (CustomSQLCTEs, MultiRowSelectNode (MultiRowSelectNode), SelectNode (SelectNode), SelectWriter (..), SourcePrefixes (SourcePrefixes), initialNativeQueryFreshIdStore)
import Hasura.Prelude
import Hasura.RQL.IR.Select (AnnAggregateSelect, AnnSelectG (_asnStrfyNum))
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.BackendType (BackendType (Postgres))
import Hasura.RQL.Types.Common (FieldName (FieldName))

-- | Translates IR to Postgres queries for aggregated SELECTs.
--
-- See 'mkAggregateSelect' for the Postgres AST.
selectAggregateQuerySQL ::
  forall pgKind.
  (Backend ('Postgres pgKind), PostgresAnnotatedFieldJSON pgKind) =>
  AnnAggregateSelect ('Postgres pgKind) ->
  Query
selectAggregateQuerySQL =
  toQuery
    . selectToSelectWith
    . mkAggregateSelect

-- | We process aggregate queries differently because the types of aggregate queries are different.
--   In the '_asnFields' field of an 'AnnSelectG', we will have a 'TableAggregateFieldG' instead
--   of an 'AnnFieldG'.
mkAggregateSelect ::
  forall pgKind m.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadWriter CustomSQLCTEs m
  ) =>
  AnnAggregateSelect ('Postgres pgKind) ->
  m Select
mkAggregateSelect annAggSel = do
  let ( (selectSource, nodeExtractors, topExtractor),
        SelectWriter {_swJoinTree = joinTree, _swCustomSQLCTEs = customSQLCTEs}
        ) =
          runWriter
            $ flip runReaderT strfyNum
            $ flip evalStateT initialNativeQueryFreshIdStore
            $ processAnnAggregateSelect sourcePrefixes rootFieldName annAggSel
      -- select the relevant columns and subquery we want to aggregate
      selectNode = SelectNode nodeExtractors joinTree
      -- aggregate the results into a top-level return value
      arrayNode = MultiRowSelectNode [topExtractor] selectNode
  tell customSQLCTEs

  pure $ generateSQLSelectFromArrayNode selectSource arrayNode $ BELit True
  where
    strfyNum = _asnStrfyNum annAggSel
    rootFieldName = FieldName "root"
    rootIdentifier = toIdentifier rootFieldName
    sourcePrefixes = SourcePrefixes rootIdentifier rootIdentifier
