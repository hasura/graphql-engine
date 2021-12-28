{-# LANGUAGE ScopedTypeVariables #-}

module Hasura.RQL.DML.Delete
  ( validateDeleteQWith,
    validateDeleteQ,
    AnnDelG (..),
    AnnDel,
    execDeleteQuery,
    runDelete,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Sequence qualified as DS
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.Execute.Mutation
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.Translate.Returning
import Hasura.Backends.Postgres.Types.Table
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.DML.Internal
import Hasura.RQL.DML.Types
import Hasura.RQL.IR.Delete
import Hasura.RQL.Types
import Hasura.Session
import Hasura.Tracing qualified as Tracing

validateDeleteQWith ::
  (UserInfoM m, QErrM m, TableInfoRM ('Postgres 'Vanilla) m) =>
  SessionVariableBuilder ('Postgres 'Vanilla) m ->
  (ColumnType ('Postgres 'Vanilla) -> Value -> m S.SQLExp) ->
  DeleteQuery ->
  m (AnnDel ('Postgres 'Vanilla))
validateDeleteQWith
  sessVarBldr
  prepValBldr
  (DeleteQuery tableName _ rqlBE mRetCols) = do
    tableInfo <- askTabInfoSource tableName
    let coreInfo = _tiCoreInfo tableInfo

    -- If table is view then check if it deletable
    mutableView
      tableName
      viIsDeletable
      (_tciViewInfo coreInfo)
      "deletable"

    -- Check if the role has delete permissions
    delPerm <- askDelPermInfo tableInfo

    -- Check if all dependent headers are present
    validateHeaders $ dpiRequiredHeaders delPerm

    -- Check if select is allowed
    selPerm <-
      modifyErr (<> selNecessaryMsg) $
        askSelPermInfo tableInfo

    let fieldInfoMap = _tciFieldInfoMap coreInfo
        allCols = getCols fieldInfoMap

    -- convert the returning cols into sql returing exp
    mAnnRetCols <- forM mRetCols $ \retCols ->
      withPathK "returning" $ checkRetCols fieldInfoMap selPerm retCols

    -- convert the where clause
    annSQLBoolExp <-
      withPathK "where" $
        convBoolExp fieldInfoMap selPerm rqlBE sessVarBldr tableName (valueParserWithCollectableType prepValBldr)

    resolvedDelFltr <-
      convAnnBoolExpPartialSQL sessVarBldr $
        dpiFilter delPerm

    return $
      AnnDel
        tableName
        (resolvedDelFltr, annSQLBoolExp)
        (mkDefaultMutFlds mAnnRetCols)
        allCols
    where
      selNecessaryMsg =
        "; \"delete\" is only allowed if the role "
          <> "has \"select\" permission as \"where\" can't be used "
          <> "without \"select\" permission on the table"

validateDeleteQ ::
  (QErrM m, UserInfoM m, CacheRM m) =>
  DeleteQuery ->
  m (AnnDel ('Postgres 'Vanilla), DS.Seq Q.PrepArg)
validateDeleteQ query = do
  let source = doSource query
  tableCache :: TableCache ('Postgres 'Vanilla) <- askTableCache source
  flip runTableCacheRT (source, tableCache) $
    runDMLP1T $
      validateDeleteQWith sessVarFromCurrentSetting binRHSBuilder query

runDelete ::
  forall m.
  ( QErrM m,
    UserInfoM m,
    CacheRM m,
    HasServerConfigCtx m,
    MonadIO m,
    Tracing.MonadTrace m,
    MonadBaseControl IO m,
    MetadataM m
  ) =>
  DeleteQuery ->
  m EncJSON
runDelete q = do
  sourceConfig <- askSourceConfig @('Postgres 'Vanilla) (doSource q)
  strfyNum <- stringifyNum . _sccSQLGenCtx <$> askServerConfigCtx
  userInfo <- askUserInfo
  validateDeleteQ q
    >>= runTxWithCtx (_pscExecCtx sourceConfig) Q.ReadWrite
      . flip runReaderT emptyQueryTagsComment
      . execDeleteQuery strfyNum userInfo
