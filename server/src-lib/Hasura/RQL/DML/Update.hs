module Hasura.RQL.DML.Update
  ( runUpdate
  ) where

import           Hasura.Prelude

import qualified Data.Environment                             as Env
import qualified Data.HashMap.Strict                          as M
import qualified Data.Sequence                                as DS
import qualified Database.PG.Query                            as Q

import           Control.Monad.Trans.Control                  (MonadBaseControl)
import           Data.Aeson.Types
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.SQL.DML             as S
import qualified Hasura.Tracing                               as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.Execute.Mutation
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.Translate.Returning
import           Hasura.Backends.Postgres.Types.Table
import           Hasura.EncJSON
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Types
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.Update
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.SQL.Types
import           Hasura.Server.Version                        (HasVersion)
import           Hasura.Session


convInc
  :: (QErrM m)
  => ValueParser 'Postgres m S.SQLExp
  -> PGCol
  -> ColumnType 'Postgres
  -> Value
  -> m (PGCol, S.SQLExp)
convInc f col colType val = do
  prepExp <- f (CollectableTypeScalar colType) val
  return (col, S.SEOpApp S.incOp [S.mkSIdenExp col, prepExp])

convMul
  :: (QErrM m)
  => ValueParser 'Postgres m S.SQLExp
  -> PGCol
  -> ColumnType 'Postgres
  -> Value
  -> m (PGCol, S.SQLExp)
convMul f col colType val = do
  prepExp <- f (CollectableTypeScalar colType) val
  return (col, S.SEOpApp S.mulOp [S.mkSIdenExp col, prepExp])

convSet
  :: (QErrM m)
  => ValueParser 'Postgres m S.SQLExp
  -> PGCol
  -> ColumnType 'Postgres
  -> Value
  -> m (PGCol, S.SQLExp)
convSet f col colType val = do
  prepExp <- f (CollectableTypeScalar colType) val
  return (col, prepExp)

convDefault :: (Monad m) => PGCol -> ColumnType 'Postgres -> () -> m (PGCol, S.SQLExp)
convDefault col _ _ = return (col, S.SEUnsafe "DEFAULT")

convOp
  :: (UserInfoM m, QErrM m)
  => FieldInfoMap (FieldInfo 'Postgres)
  -> [PGCol]
  -> UpdPermInfo 'Postgres
  -> [(PGCol, a)]
  -> (PGCol -> ColumnType 'Postgres -> a -> m (PGCol, S.SQLExp))
  -> m [(PGCol, S.SQLExp)]
convOp fieldInfoMap preSetCols updPerm objs conv =
  forM objs $ \(pgCol, a) -> do
    -- if column has predefined value then throw error
    when (pgCol `elem` preSetCols) $ throwNotUpdErr pgCol
    checkPermOnCol PTUpdate allowedCols pgCol
    colType <- askColumnType fieldInfoMap pgCol relWhenPgErr
    res <- conv pgCol colType a
    -- build a set expression's entry
    withPathK (getPGColTxt pgCol) $ return res
  where
    allowedCols  = upiCols updPerm
    relWhenPgErr = "relationships can't be updated"
    throwNotUpdErr c = do
      roleName <- _uiRole <$> askUserInfo
      throw400 NotSupported $ "column " <> c <<> " is not updatable"
        <> " for role " <> roleName <<> "; its value is predefined in permission"

validateUpdateQueryWith
  :: (UserInfoM m, QErrM m, TableInfoRM 'Postgres m)
  => SessVarBldr 'Postgres m
  -> ValueParser 'Postgres m S.SQLExp
  -> UpdateQuery
  -> m (AnnUpd 'Postgres)
validateUpdateQueryWith sessVarBldr prepValBldr uq = do
  let tableName = uqTable uq
  tableInfo <- withPathK "table" $ askTabInfoSource tableName
  let coreInfo = _tiCoreInfo tableInfo

  -- If it is view then check if it is updatable
  mutableView tableName viIsUpdatable
    (_tciViewInfo coreInfo) "updatable"

  -- Check if the role has update permissions
  updPerm <- askUpdPermInfo tableInfo

  -- Check if all dependent headers are present
  validateHeaders $ upiRequiredHeaders updPerm

  -- Check if select is allowed
  selPerm <- modifyErr (<> selNecessaryMsg) $
             askSelPermInfo tableInfo

  let fieldInfoMap = _tciFieldInfoMap coreInfo
      allCols = getCols fieldInfoMap
      preSetObj = upiSet updPerm
      preSetCols = M.keys preSetObj

  -- convert the object to SQL set expression
  setItems <- withPathK "$set" $
    convOp fieldInfoMap preSetCols updPerm (M.toList $ uqSet uq) $ convSet prepValBldr

  incItems <- withPathK "$inc" $
    convOp fieldInfoMap preSetCols updPerm (M.toList $ uqInc uq) $ convInc prepValBldr

  mulItems <- withPathK "$mul" $
    convOp fieldInfoMap preSetCols updPerm (M.toList $ uqMul uq) $ convMul prepValBldr

  defItems <- withPathK "$default" $
    convOp fieldInfoMap preSetCols updPerm ((,()) <$> uqDefault uq) convDefault

  -- convert the returning cols into sql returing exp
  mAnnRetCols <- forM mRetCols $ \retCols ->
    withPathK "returning" $ checkRetCols fieldInfoMap selPerm retCols

  resolvedPreSetItems <- M.toList <$>
                         mapM (convPartialSQLExp sessVarBldr) preSetObj

  let setExpItems = resolvedPreSetItems ++
                    setItems ++
                    incItems ++
                    mulItems ++
                    defItems

  when (null setExpItems) $
    throw400 UnexpectedPayload "atleast one of $set, $inc, $mul has to be present"

  -- convert the where clause
  annSQLBoolExp <- withPathK "where" $
    convBoolExp fieldInfoMap selPerm (uqWhere uq) sessVarBldr tableName prepValBldr

  resolvedUpdFltr <- convAnnBoolExpPartialSQL sessVarBldr $
                     upiFilter updPerm
  resolvedUpdCheck <- fromMaybe gBoolExpTrue <$>
                        traverse (convAnnBoolExpPartialSQL sessVarBldr)
                          (upiCheck updPerm)

  return $ AnnUpd
    tableName
    (fmap UpdSet <$> setExpItems)
    (resolvedUpdFltr, annSQLBoolExp)
    resolvedUpdCheck
    (mkDefaultMutFlds mAnnRetCols)
    allCols
  where
    mRetCols = uqReturning uq
    selNecessaryMsg =
      "; \"update\" is only allowed if the role "
      <> "has \"select\" permission as \"where\" can't be used "
      <> "without \"select\" permission on the table"

validateUpdateQuery
  :: (QErrM m, UserInfoM m, CacheRM m)
  => UpdateQuery -> m (AnnUpd 'Postgres, DS.Seq Q.PrepArg)
validateUpdateQuery query = do
  let source = uqSource query
  tableCache :: TableCache 'Postgres <- askTableCache source
  flip runTableCacheRT (source, tableCache) $ runDMLP1T $
    validateUpdateQueryWith sessVarFromCurrentSetting (valueParserWithCollectableType binRHSBuilder) query

runUpdate
  :: ( HasVersion, QErrM m, UserInfoM m, CacheRM m
     , HasServerConfigCtx m, MonadBaseControl IO m
     , MonadIO m, Tracing.MonadTrace m, MetadataM m
     )
  => Env.Environment -> UpdateQuery -> m EncJSON
runUpdate env q = do
  sourceConfig <- askSourceConfig (uqSource q)
  strfyNum <- stringifyNum . _sccSQLGenCtx <$> askServerConfigCtx
  validateUpdateQuery q
    >>= runQueryLazyTx (_pscExecCtx sourceConfig) Q.ReadWrite
        . execUpdateQuery env strfyNum Nothing
