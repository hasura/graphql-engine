-- | This module defines the top-level translation functions pertaining to
-- queries that use aggregation (i.e., /not/ so-called "simple" selects) into
-- Postgres AST.
module Hasura.Backends.Postgres.Translate.Select.Aggregate
  ( mkAggregateSelect,
    selectAggregateQuerySQL,
  )
where

import Control.Monad.Writer.Strict (runWriter)
import Database.PG.Query (Query, fromBuilder)
import Hasura.Backends.Postgres.SQL.DML (BoolExp (BELit), Select)
import Hasura.Backends.Postgres.SQL.IdentifierUniqueness
  ( prefixNumToAliases,
  )
import Hasura.Backends.Postgres.SQL.Types
  ( IsIdentifier (toIdentifier),
  )
import Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON
import Hasura.Backends.Postgres.Translate.Select.Internal.GenerateSelect (generateSQLSelectFromArrayNode)
import Hasura.Backends.Postgres.Translate.Select.Internal.Process (processAnnAggregateSelect)
import Hasura.Backends.Postgres.Translate.Types
  ( MultiRowSelectNode (MultiRowSelectNode),
    SelectNode (SelectNode),
    SourcePrefixes (SourcePrefixes),
  )
import Hasura.Prelude
import Hasura.RQL.IR.Select
  ( AnnAggregateSelect,
    AnnSelectG (_asnStrfyNum),
  )
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.Common (FieldName (FieldName))
import Hasura.SQL.Backend (BackendType (Postgres))
import Hasura.SQL.Types (ToSQL (toSQL))

-- | Translates IR to Postgres queries for aggregated SELECTs.
--
-- See 'mkAggregateSelect' for the Postgres AST.
selectAggregateQuerySQL ::
  forall pgKind.
  (Backend ('Postgres pgKind), PostgresAnnotatedFieldJSON pgKind) =>
  AnnAggregateSelect ('Postgres pgKind) ->
  Query
selectAggregateQuerySQL =
  fromBuilder . toSQL . mkAggregateSelect

mkAggregateSelect ::
  forall pgKind.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  AnnAggregateSelect ('Postgres pgKind) ->
  Select
mkAggregateSelect annAggSel =
  let ((selectSource, nodeExtractors, topExtractor), joinTree) =
        runWriter $
          flip runReaderT strfyNum $
            processAnnAggregateSelect sourcePrefixes rootFieldName annAggSel
      selectNode = SelectNode nodeExtractors joinTree
      arrayNode = MultiRowSelectNode [topExtractor] selectNode
   in prefixNumToAliases $
        generateSQLSelectFromArrayNode selectSource arrayNode $ BELit True
  where
    strfyNum = _asnStrfyNum annAggSel
    rootFieldName = FieldName "root"
    rootIdentifier = toIdentifier rootFieldName
    sourcePrefixes = SourcePrefixes rootIdentifier rootIdentifier
