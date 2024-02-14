module Hasura.RQL.DML.Delete
  ( validateDeleteQWith,
    validateDeleteQ,
    AnnDelG (..),
    AnnDel,
    execDeleteQuery,
    runDelete,
  )
where

import Control.Lens ((^?))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Sequence qualified as DS
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.Execute.Mutation
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.Translate.Returning
import Hasura.Backends.Postgres.Types.Table
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.LogicalModel.Cache (LogicalModelCache, LogicalModelInfo (..))
import Hasura.LogicalModel.Fields (LogicalModelFieldsRM, runLogicalModelFieldsLookup)
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.DML.Internal
import Hasura.RQL.DML.Types
import Hasura.RQL.IR.Delete
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.SchemaCache
import Hasura.Session
import Hasura.Tracing qualified as Tracing

validateDeleteQWith ::
  (UserInfoM m, QErrM m, TableInfoRM ('Postgres 'Vanilla) m, LogicalModelFieldsRM ('Postgres 'Vanilla) m) =>
  SessionVariableBuilder m ->
  (ColumnType ('Postgres 'Vanilla) -> Value -> m S.SQLExp) ->
  DeleteQuery ->
  m (AnnDel ('Postgres 'Vanilla))
validateDeleteQWith
  sessVarBldr
  prepValBldr
  (DeleteQuery tableName _ rqlBE mRetCols) = do
    tableInfo <- askTableInfoSource tableName
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
      modifyErr (<> selNecessaryMsg)
        $ askSelPermInfo tableInfo

    let fieldInfoMap = _tciFieldInfoMap coreInfo
        allCols = mapMaybe (^? _SCIScalarColumn) $ getCols fieldInfoMap

    -- convert the returning cols into sql returing exp
    mAnnRetCols <- forM mRetCols $ \retCols ->
      withPathK "returning" $ checkRetCols fieldInfoMap selPerm retCols

    -- convert the where clause
    annSQLBoolExp <-
      withPathK "where"
        $ convBoolExp fieldInfoMap selPerm rqlBE sessVarBldr fieldInfoMap (valueParserWithCollectableType prepValBldr)

    resolvedDelFltr <-
      convAnnBoolExpPartialSQL sessVarBldr
        $ dpiFilter delPerm

    let validateInput = dpiValidateInput delPerm

    return
      $ AnnDel
        tableName
        (resolvedDelFltr, annSQLBoolExp)
        (mkDefaultMutFlds mAnnRetCols)
        allCols
        Nothing
        validateInput
        False
    where
      selNecessaryMsg =
        "; \"delete\" is only allowed if the role "
          <> "has \"select\" permission as \"where\" can't be used "
          <> "without \"select\" permission on the table"

validateDeleteQ ::
  (QErrM m, UserInfoM m, CacheRM m) =>
  DeleteQuery ->
  m (AnnDel ('Postgres 'Vanilla), DS.Seq PG.PrepArg)
validateDeleteQ query = do
  let source = doSource query
  tableCache :: TableCache ('Postgres 'Vanilla) <- fold <$> askTableCache source
  logicalModelCache :: LogicalModelCache ('Postgres 'Vanilla) <- fold <$> askLogicalModelCache source
  flip runTableCacheRT tableCache
    $ runLogicalModelFieldsLookup _lmiFields logicalModelCache
    $ runDMLP1T
    $ validateDeleteQWith sessVarFromCurrentSetting binRHSBuilder query

runDelete ::
  forall m.
  ( QErrM m,
    UserInfoM m,
    CacheRM m,
    MonadIO m,
    Tracing.MonadTrace m,
    MonadBaseControl IO m,
    MetadataM m
  ) =>
  SQLGenCtx ->
  DeleteQuery ->
  m EncJSON
runDelete sqlGen q = do
  sourceConfig <- askSourceConfig @('Postgres 'Vanilla) (doSource q)
  let strfyNum = stringifyNum sqlGen
  userInfo <- askUserInfo
  validateDeleteQ q
    >>= runTxWithCtx (_pscExecCtx sourceConfig) (Tx PG.ReadWrite Nothing) LegacyRQLQuery
    . flip runReaderT emptyQueryTagsComment
    . execDeleteQuery strfyNum Nothing userInfo
