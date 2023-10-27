-- | This module defines the top-level translation functions pertaining to
-- queries that are not aggregation queries, i.e. so-called "simple" selects
-- into Postgres AST.
module Hasura.Backends.Postgres.Translate.Select.Simple
  ( mkSQLSelect,
    selectQuerySQL,
  )
where

-- import Control.Monad.Writer.Strict (runWriter)
import Database.PG.Query (Query)
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types (IsIdentifier (toIdentifier))
import Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON
import Hasura.Backends.Postgres.Translate.Select.Internal.Extractor (asJsonAggExtr)
import Hasura.Backends.Postgres.Translate.Select.Internal.GenerateSelect (PostgresGenerateSQLSelect, generateSQLSelectFromArrayNode)
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers (selectToSelectWithM, toQuery)
import Hasura.Backends.Postgres.Translate.Select.Internal.Process (processAnnSimpleSelect)
import Hasura.Backends.Postgres.Translate.Types
import Hasura.Base.Error (QErr)
import Hasura.Prelude
import Hasura.RQL.IR.Select
  ( AnnSelectG (_asnStrfyNum),
    AnnSimpleSelect,
  )
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.BackendType (BackendType (Postgres))
import Hasura.RQL.Types.Common
  ( FieldName (FieldName),
    JsonAggSelect,
  )
import Hasura.RQL.Types.Session (UserInfo)

-- | Translates IR to Postgres queries for simple SELECTs (select queries that
-- are not aggregations, including subscriptions).
--
-- See 'mkSQLSelect' for the Postgres AST.
selectQuerySQL ::
  forall pgKind m.
  (Backend ('Postgres pgKind), PostgresAnnotatedFieldJSON pgKind, PostgresGenerateSQLSelect pgKind, MonadIO m, MonadError QErr m) =>
  UserInfo ->
  JsonAggSelect ->
  AnnSimpleSelect ('Postgres pgKind) ->
  m Query
selectQuerySQL userInfo jsonAggSelect annSimpleSelect = do
  selectWithQuery <- selectToSelectWithM . mkSQLSelect userInfo jsonAggSelect $ annSimpleSelect
  pure $ toQuery selectWithQuery

mkSQLSelect ::
  forall pgKind m.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    PostgresGenerateSQLSelect pgKind,
    MonadWriter CustomSQLCTEs m,
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  JsonAggSelect ->
  AnnSimpleSelect ('Postgres pgKind) ->
  m S.Select
mkSQLSelect userInfo jsonAggSelect annSel = do
  let permLimitSubQuery = PLSQNotRequired
  -- run an intermediary step in order to obtain:
  -- SelectSource: the primary source, along with its where clause and sorting/slicing information
  -- NodeExtractors: a map from aliases which need to be selected to the corresponding required SQLExp to select the value
  -- : the join tree required for relationships (built via @MonadWriter@)
  -- : any top-level Common Table Expressions needed for Native Queries
  ((selectSource, nodeExtractors), SelectWriter {_swJoinTree = joinTree, _swCustomSQLCTEs = customSQLCTEs}) <-
    runWriterT
      $ flip runReaderT strfyNum
      $ flip evalStateT initialNativeQueryFreshIdStore
      $ processAnnSimpleSelect userInfo sourcePrefixes rootFldName permLimitSubQuery annSel

  let selectNode = SelectNode nodeExtractors joinTree
      topExtractor =
        asJsonAggExtr jsonAggSelect rootFldAls permLimitSubQuery
          $ orderByForJsonAgg selectSource
      arrayNode = MultiRowSelectNode [topExtractor] selectNode
  tell customSQLCTEs

  pure $ generateSQLSelectFromArrayNode @pgKind selectSource arrayNode $ S.BELit True
  where
    strfyNum = _asnStrfyNum annSel
    rootFldIdentifier = toIdentifier rootFldName
    sourcePrefixes = SourcePrefixes rootFldIdentifier rootFldIdentifier
    rootFldName = FieldName "root"
    rootFldAls = S.toColumnAlias $ toIdentifier rootFldName
