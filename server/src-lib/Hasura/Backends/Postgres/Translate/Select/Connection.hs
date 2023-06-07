-- | This module defines the top-level translation functions pertaining to
-- translating Connection (i.e. Relay) queries into Postgres AST.
module Hasura.Backends.Postgres.Translate.Select.Connection
  ( connectionSelectQuerySQL,
    mkConnectionSelect,
  )
where

import Control.Monad.Writer (runWriter)
import Database.PG.Query (Query)
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON
import Hasura.Backends.Postgres.Translate.Select.Internal.GenerateSelect (connectionToSelectWith)
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers (customSQLToTopLevelCTEs, toQuery)
import Hasura.Backends.Postgres.Translate.Select.Internal.Process (processConnectionSelect)
import Hasura.Backends.Postgres.Translate.Types
import Hasura.Prelude
import Hasura.RQL.IR.Select
  ( AnnSelectG (_asnStrfyNum),
    ConnectionSelect (_csSelect),
  )
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.BackendType (BackendType (Postgres))
import Hasura.RQL.Types.Common (FieldName (FieldName))

-- | Translates IR to Postgres queries for "connection" queries (used for Relay).
--
-- See 'mkConnectionSelect' for the Postgres AST.
connectionSelectQuerySQL ::
  forall pgKind.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  ConnectionSelect ('Postgres pgKind) Void S.SQLExp ->
  Query
connectionSelectQuerySQL =
  toQuery
    . ( \(selectWith, customCTEs) ->
          selectWith
            { S.swCTEs =
                map (fmap S.CTESelect) (S.swCTEs selectWith)
                  <> customSQLToTopLevelCTEs customCTEs
            }
      )
    . runWriter
    . mkConnectionSelect

mkConnectionSelect ::
  forall pgKind m.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadWriter CustomSQLCTEs m
  ) =>
  ConnectionSelect ('Postgres pgKind) Void S.SQLExp ->
  m (S.SelectWithG S.Select)
mkConnectionSelect connectionSelect = do
  let ( (connectionSource, topExtractor, nodeExtractors),
        SelectWriter {_swJoinTree = joinTree, _swCustomSQLCTEs = customSQLCTEs}
        ) =
          runWriter
            $ flip evalStateT initialNativeQueryFreshIdStore
            $ flip runReaderT strfyNum
            $ processConnectionSelect
              sourcePrefixes
              rootFieldName
              (S.toTableAlias rootIdentifier)
              mempty
              connectionSelect
      selectNode =
        MultiRowSelectNode [topExtractor]
          $ SelectNode nodeExtractors joinTree
      selectWith =
        connectionToSelectWith (S.toTableAlias rootIdentifier) connectionSource selectNode
  tell customSQLCTEs

  pure selectWith
  where
    strfyNum = _asnStrfyNum $ _csSelect connectionSelect
    rootFieldName = FieldName "root"
    rootIdentifier = toIdentifier rootFieldName
    sourcePrefixes = SourcePrefixes rootIdentifier rootIdentifier
