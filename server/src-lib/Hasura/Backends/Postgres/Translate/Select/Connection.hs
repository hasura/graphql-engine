-- | This module defines the top-level translation functions pertaining to
-- translating Connection (i.e. Relay) queries into Postgres AST.
module Hasura.Backends.Postgres.Translate.Select.Connection
  ( connectionSelectQuerySQL,
    mkConnectionSelect,
  )
where

-- import Control.Monad.Writer (runWriter)
import Database.PG.Query (Query)
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON
import Hasura.Backends.Postgres.Translate.Select.Internal.GenerateSelect (PostgresGenerateSQLSelect, connectionToSelectWith)
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers (customSQLToTopLevelCTEs, toQuery)
import Hasura.Backends.Postgres.Translate.Select.Internal.Process (processConnectionSelect)
import Hasura.Backends.Postgres.Translate.Types
import Hasura.Base.Error (QErr)
import Hasura.Prelude
import Hasura.RQL.IR.Select
  ( AnnSelectG (_asnStrfyNum),
    ConnectionSelect (_csSelect),
  )
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.BackendType (BackendType (Postgres))
import Hasura.RQL.Types.Common (FieldName (FieldName))
import Hasura.RQL.Types.Session (UserInfo)

-- | Translates IR to Postgres queries for "connection" queries (used for Relay).
--
-- See 'mkConnectionSelect' for the Postgres AST.
connectionSelectQuerySQL ::
  forall pgKind m.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    PostgresGenerateSQLSelect pgKind,
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  ConnectionSelect ('Postgres pgKind) Void S.SQLExp ->
  m Query
connectionSelectQuerySQL userInfo connSet = do
  selectWithGAndCTEs <-
    runWriterT
      . mkConnectionSelect userInfo
      $ connSet
  pure
    $ toQuery
    . ( \(selectWith, customCTEs) ->
          selectWith
            { S.swCTEs =
                map (fmap S.CTESelect) (S.swCTEs selectWith)
                  <> customSQLToTopLevelCTEs customCTEs
            }
      )
    $ selectWithGAndCTEs

mkConnectionSelect ::
  forall pgKind m.
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    PostgresGenerateSQLSelect pgKind,
    MonadWriter CustomSQLCTEs m,
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  ConnectionSelect ('Postgres pgKind) Void S.SQLExp ->
  m (S.SelectWithG S.Select)
mkConnectionSelect userInfo connectionSelect = do
  ( (connectionSource, topExtractor, nodeExtractors),
    SelectWriter {_swJoinTree = joinTree, _swCustomSQLCTEs = customSQLCTEs}
    ) <-
    runWriterT
      $ flip evalStateT initialNativeQueryFreshIdStore
      $ flip runReaderT strfyNum
      $ processConnectionSelect
        userInfo
        sourcePrefixes
        rootFieldName
        (S.toTableAlias rootIdentifier)
        mempty
        connectionSelect
  let selectNode =
        MultiRowSelectNode [topExtractor]
          $ SelectNode nodeExtractors joinTree
      selectWith =
        connectionToSelectWith @pgKind (S.toTableAlias rootIdentifier) connectionSource selectNode
  tell customSQLCTEs

  pure selectWith
  where
    strfyNum = _asnStrfyNum $ _csSelect connectionSelect
    rootFieldName = FieldName "root"
    rootIdentifier = toIdentifier rootFieldName
    sourcePrefixes = SourcePrefixes rootIdentifier rootIdentifier
