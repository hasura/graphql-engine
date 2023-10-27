-- | This module defines the top-level translation functions pertaining to
-- queries that use aggregation (i.e., /not/ so-called "simple" selects) into
-- Postgres AST.
module Hasura.Backends.Postgres.Translate.Select.Aggregate
  ( mkAggregateSelect,
    selectAggregateQuerySQL,
  )
where

import Database.PG.Query (Query)
import Hasura.Backends.Postgres.SQL.DML (BoolExp (BELit), Select)
import Hasura.Backends.Postgres.SQL.Types (IsIdentifier (toIdentifier))
import Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON
import Hasura.Backends.Postgres.Translate.Select.Internal.GenerateSelect (PostgresGenerateSQLSelect, generateSQLSelectFromArrayNode)
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers (selectToSelectWithM, toQuery)
import Hasura.Backends.Postgres.Translate.Select.Internal.Process (processAnnAggregateSelect)
import Hasura.Backends.Postgres.Translate.Types (CustomSQLCTEs, MultiRowSelectNode (MultiRowSelectNode), SelectNode (SelectNode), SelectWriter (..), SourcePrefixes (SourcePrefixes), initialNativeQueryFreshIdStore)
import Hasura.Base.Error (QErr)
import Hasura.Prelude
import Hasura.RQL.IR.Select (AnnAggregateSelect, AnnSelectG (_asnStrfyNum))
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.BackendType (BackendType (Postgres))
import Hasura.RQL.Types.Common (FieldName (FieldName))
import Hasura.RQL.Types.Session (UserInfo)

-- | Translates IR to Postgres queries for aggregated SELECTs.
--
-- See 'mkAggregateSelect' for the Postgres AST.
selectAggregateQuerySQL ::
  forall pgKind m.
  (Backend ('Postgres pgKind), PostgresAnnotatedFieldJSON pgKind, PostgresGenerateSQLSelect pgKind, MonadIO m, MonadError QErr m) =>
  UserInfo ->
  AnnAggregateSelect ('Postgres pgKind) ->
  m Query
selectAggregateQuerySQL userInfo selectQuery = do
  selectWithExp <- selectToSelectWithM . mkAggregateSelect userInfo $ selectQuery
  pure $ toQuery selectWithExp

-- | We process aggregate queries differently because the types of aggregate queries are different.
--   In the '_asnFields' field of an 'AnnSelectG', we will have a 'TableAggregateFieldG' instead
--   of an 'AnnFieldG'.
mkAggregateSelect ::
  forall pgKind m.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    PostgresGenerateSQLSelect pgKind,
    MonadWriter CustomSQLCTEs m,
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  AnnAggregateSelect ('Postgres pgKind) ->
  m Select
mkAggregateSelect userInfo annAggSel = do
  ( (selectSource, nodeExtractors, topExtractor),
    SelectWriter {_swJoinTree = joinTree, _swCustomSQLCTEs = customSQLCTEs}
    ) <-
    runWriterT
      $ flip runReaderT strfyNum
      $ flip evalStateT initialNativeQueryFreshIdStore
      $ processAnnAggregateSelect userInfo sourcePrefixes rootFieldName annAggSel
  -- select the relevant columns and subquery we want to aggregate
  let selectNode = SelectNode nodeExtractors joinTree
      -- aggregate the results into a top-level return value
      arrayNode = MultiRowSelectNode [topExtractor] selectNode
  tell customSQLCTEs

  pure $ generateSQLSelectFromArrayNode @pgKind selectSource arrayNode $ BELit True
  where
    strfyNum = _asnStrfyNum annAggSel
    rootFieldName = FieldName "root"
    rootIdentifier = toIdentifier rootFieldName
    sourcePrefixes = SourcePrefixes rootIdentifier rootIdentifier
