module Hasura.RQL.DML.Internal
  ( SessionVariableBuilder,
    askDelPermInfo,
    askInsPermInfo,
    askPermInfo,
    askSelPermInfo,
    askTableInfoSource,
    askUpdPermInfo,
    binRHSBuilder,
    checkPermOnCol,
    checkRetCols,
    checkSelOnCol,
    convAnnBoolExpPartialSQL,
    convAnnRedactionExpPartialSQL,
    convBoolExp,
    convPartialSQLExp,
    fetchRelDet,
    fetchRelTabInfo,
    isTabUpdatable,
    onlyPositiveInt,
    runDMLP1T,
    sessVarFromCurrentSetting,
    validateHeaders,
    valueParserWithCollectableType,
    verifyAsrns,
  )
where

import Control.Lens
import Data.Aeson.Types
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HS
import Data.Sequence qualified as DS
import Data.Text qualified as T
import Data.Text.Extended
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Instances.Metadata ()
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types hiding (TableName)
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Backends.Postgres.Translate.Column
import Hasura.Backends.Postgres.Types.Column
import Hasura.Base.Error
import Hasura.LogicalModel.Fields (LogicalModelFieldsRM)
import Hasura.Prelude
import Hasura.RQL.DDL.Permission (annBoolExp)
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.BoolExp
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Roles (RoleName, adminRoleName)
import Hasura.RQL.Types.SchemaCache
import Hasura.SQL.Types
import Hasura.Session (SessionVariable, UserInfoM, askCurRole, askUserInfo, getSessionVariables, sessionVariableToText, _uiSession)
import Hasura.Table.Cache

newtype DMLP1T m a = DMLP1T {unDMLP1T :: StateT (DS.Seq PG.PrepArg) m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadTrans,
      MonadState (DS.Seq PG.PrepArg),
      MonadError e,
      TableCoreInfoRM b,
      TableInfoRM b,
      LogicalModelFieldsRM b,
      CacheRM,
      UserInfoM
    )

runDMLP1T :: DMLP1T m a -> m (a, DS.Seq PG.PrepArg)
runDMLP1T = flip runStateT DS.empty . unDMLP1T

askPermInfo ::
  (UserInfoM m) =>
  Lens' (RolePermInfo ('Postgres 'Vanilla)) (Maybe c) ->
  TableInfo ('Postgres 'Vanilla) ->
  m (Maybe c)
askPermInfo pa tableInfo = do
  role <- askCurRole
  return $ getPermInfoMaybe role pa tableInfo

getPermInfoMaybe ::
  RoleName -> Lens' (RolePermInfo ('Postgres 'Vanilla)) (Maybe c) -> TableInfo ('Postgres 'Vanilla) -> Maybe c
getPermInfoMaybe role pa tableInfo =
  getRolePermInfo role tableInfo ^. pa

assertAskPermInfo ::
  (UserInfoM m, QErrM m) =>
  PermType ->
  Lens' (RolePermInfo ('Postgres 'Vanilla)) (Maybe c) ->
  TableInfo ('Postgres 'Vanilla) ->
  m c
assertAskPermInfo pt pa tableInfo = do
  roleName <- askCurRole
  mPermInfo <- askPermInfo pa tableInfo
  onNothing mPermInfo
    $ throw400 PermissionDenied
    $ permTypeToCode pt
    <> " on "
    <> tableInfoName tableInfo
    <<> " for role "
    <> roleName
    <<> " is not allowed. "

isTabUpdatable :: RoleName -> TableInfo ('Postgres 'Vanilla) -> Bool
isTabUpdatable role ti
  | role == adminRoleName = True
  | otherwise = isJust $ HashMap.lookup role rpim >>= _permUpd
  where
    rpim = _tiRolePermInfoMap ti

askInsPermInfo ::
  (UserInfoM m, QErrM m) =>
  TableInfo ('Postgres 'Vanilla) ->
  m (InsPermInfo ('Postgres 'Vanilla))
askInsPermInfo = assertAskPermInfo PTInsert permIns

askSelPermInfo ::
  (UserInfoM m, QErrM m) =>
  TableInfo ('Postgres 'Vanilla) ->
  m (SelPermInfo ('Postgres 'Vanilla))
askSelPermInfo = assertAskPermInfo PTSelect permSel

askUpdPermInfo ::
  (UserInfoM m, QErrM m) =>
  TableInfo ('Postgres 'Vanilla) ->
  m (UpdPermInfo ('Postgres 'Vanilla))
askUpdPermInfo = assertAskPermInfo PTUpdate permUpd

askDelPermInfo ::
  (UserInfoM m, QErrM m) =>
  TableInfo ('Postgres 'Vanilla) ->
  m (DelPermInfo ('Postgres 'Vanilla))
askDelPermInfo = assertAskPermInfo PTDelete permDel

verifyAsrns :: (MonadError QErr m) => [a -> m ()] -> [a] -> m ()
verifyAsrns preds xs = indexedForM_ xs $ \a -> mapM_ ($ a) preds

checkRetCols ::
  (UserInfoM m, QErrM m) =>
  FieldInfoMap (FieldInfo ('Postgres 'Vanilla)) ->
  SelPermInfo ('Postgres 'Vanilla) ->
  [PGCol] ->
  m [ColumnInfo ('Postgres 'Vanilla)]
checkRetCols fieldInfoMap selPermInfo cols = do
  mapM_ (checkSelOnCol selPermInfo) cols
  forM cols $ \col -> askColInfo fieldInfoMap col relInRetErr
  where
    relInRetErr = "Relationships can't be used in \"returning\"."

checkSelOnCol ::
  (UserInfoM m, QErrM m) =>
  SelPermInfo ('Postgres 'Vanilla) ->
  Column ('Postgres 'Vanilla) ->
  m ()
checkSelOnCol selPermInfo =
  checkPermOnCol PTSelect (HS.fromList $ HashMap.keys $ spiCols selPermInfo)

checkPermOnCol ::
  (UserInfoM m, QErrM m) =>
  PermType ->
  HS.HashSet (Column ('Postgres 'Vanilla)) ->
  Column ('Postgres 'Vanilla) ->
  m ()
checkPermOnCol pt allowedCols col = do
  role <- askCurRole
  unless (HS.member col allowedCols)
    $ throw400 PermissionDenied
    $ permErrMsg role
  where
    permErrMsg role
      | role == adminRoleName = "no such column exists: " <>> col
      | otherwise =
          "role " <> role <<> " does not have permission to " <> permTypeToCode pt <> " column " <>> col

checkSelectPermOnScalarComputedField ::
  (UserInfoM m, QErrM m) =>
  SelPermInfo ('Postgres 'Vanilla) ->
  ComputedFieldName ->
  m ()
checkSelectPermOnScalarComputedField selPermInfo computedField = do
  role <- askCurRole
  unless (HashMap.member computedField $ spiComputedFields selPermInfo)
    $ throw400 PermissionDenied
    $ permErrMsg role
  where
    permErrMsg role
      | role == adminRoleName = "no such computed field exists: " <>> computedField
      | otherwise =
          "role " <> role <<> " does not have permission to select computed field" <>> computedField

valueParserWithCollectableType ::
  (MonadError QErr m) =>
  (ColumnType ('Postgres 'Vanilla) -> Value -> m S.SQLExp) ->
  CollectableType (ColumnType ('Postgres 'Vanilla)) ->
  Value ->
  m S.SQLExp
valueParserWithCollectableType valBldr pgType val = case pgType of
  CollectableTypeScalar ty -> valBldr ty val
  CollectableTypeArray ofTy -> do
    -- for arrays, we don't use the prepared builder
    vals <- runAesonParser parseJSON val
    scalarValues <- parseScalarValuesColumnTypeWithContext () ofTy vals
    return
      $ S.SETyAnn
        (S.SEArray $ map (toTxtValue . ColumnValue ofTy) scalarValues)
        (S.mkTypeAnn $ CollectableTypeArray (unsafePGColumnToBackend ofTy))

binRHSBuilder ::
  (QErrM m) =>
  ColumnType ('Postgres 'Vanilla) ->
  Value ->
  DMLP1T m S.SQLExp
binRHSBuilder colType val = do
  preparedArgs <- get
  scalarValue <- parseScalarValueColumnTypeWithContext () colType val
  put (preparedArgs DS.|> binEncoder scalarValue)
  return $ toPrepParam (DS.length preparedArgs + 1) (unsafePGColumnToBackend colType)

fetchRelTabInfo ::
  (QErrM m, TableInfoRM ('Postgres 'Vanilla) m) =>
  TableName ('Postgres 'Vanilla) ->
  m (TableInfo ('Postgres 'Vanilla))
fetchRelTabInfo refTabName =
  -- Internal error
  modifyErrAndSet500 ("foreign " <>)
    $ askTableInfoSource refTabName

askTableInfoSource ::
  (QErrM m, TableInfoRM ('Postgres 'Vanilla) m) =>
  TableName ('Postgres 'Vanilla) ->
  m (TableInfo ('Postgres 'Vanilla))
askTableInfoSource tableName = do
  onNothingM (lookupTableInfo tableName)
    $ throw400 NotExists
    $ "table "
    <> tableName
    <<> " does not exist"

data SessionVariableBuilder m = SessionVariableBuilder
  { _svbCurrentSession :: SQLExpression ('Postgres 'Vanilla),
    _svbVariableParser :: SessionVarType ('Postgres 'Vanilla) -> SessionVariable -> m (SQLExpression ('Postgres 'Vanilla))
  }

fetchRelDet ::
  (UserInfoM m, QErrM m, TableInfoRM ('Postgres 'Vanilla) m) =>
  RelName ->
  TableName ('Postgres 'Vanilla) ->
  m (FieldInfoMap (FieldInfo ('Postgres 'Vanilla)), SelPermInfo ('Postgres 'Vanilla))
fetchRelDet relName refTabName = do
  roleName <- askCurRole
  -- Internal error
  refTabInfo <- fetchRelTabInfo refTabName
  -- Get the correct constraint that applies to the given relationship
  refSelPerm <-
    modifyErr (relPermErr refTabName roleName)
      $ askSelPermInfo refTabInfo

  return (_tciFieldInfoMap $ _tiCoreInfo refTabInfo, refSelPerm)
  where
    relPermErr rTable roleName _ =
      "role "
        <> roleName
        <<> " does not have permission to read relationship "
        <> relName
        <<> "; no permission on table "
        <>> rTable

checkOnColExp ::
  (UserInfoM m, QErrM m, TableInfoRM ('Postgres 'Vanilla) m) =>
  SelPermInfo ('Postgres 'Vanilla) ->
  SessionVariableBuilder m ->
  AnnBoolExpFldSQL ('Postgres 'Vanilla) ->
  m (AnnBoolExpFldSQL ('Postgres 'Vanilla))
checkOnColExp spi sessVarBldr annFld = case annFld of
  AVColumn colInfo _ _ -> do
    let cn = ciColumn colInfo
    checkSelOnCol spi cn
    return annFld
  AVRelationship relInfo (RelationshipFilters targetPerm nesAnn) ->
    case riTarget relInfo of
      RelTargetNativeQuery _ -> error "checkOnColExp RelTargetNativeQuery"
      RelTargetTable tableName -> do
        relSPI <- snd <$> fetchRelDet (riName relInfo) tableName
        modAnn <- checkSelPerm relSPI sessVarBldr nesAnn
        resolvedFltr <- convAnnBoolExpPartialSQL sessVarBldr $ spiFilter relSPI
        return $ AVRelationship relInfo (RelationshipFilters targetPerm (andAnnBoolExps modAnn resolvedFltr))
  AVComputedField cfBoolExp -> do
    roleName <- askCurRole
    let fieldName = _acfbName cfBoolExp
    case _acfbBoolExp cfBoolExp of
      CFBEScalar _ _ -> do
        checkSelectPermOnScalarComputedField spi fieldName
        pure annFld
      CFBETable table nesBoolExp -> do
        tableInfo <- modifyErrAndSet500 ("function " <>) $ askTableInfoSource table
        let errMsg _ =
              "role "
                <> roleName
                <<> " does not have permission to read "
                <> " computed field "
                <> fieldName
                <<> "; no permission on table "
                <>> table
        tableSPI <- modifyErr errMsg $ askSelPermInfo tableInfo
        modBoolExp <- checkSelPerm tableSPI sessVarBldr nesBoolExp
        resolvedFltr <- convAnnBoolExpPartialSQL sessVarBldr $ spiFilter tableSPI
        -- Including table permission filter; "input condition" AND "permission filter condition"
        let finalBoolExp = andAnnBoolExps modBoolExp resolvedFltr
        pure $ AVComputedField cfBoolExp {_acfbBoolExp = CFBETable table finalBoolExp}
  AVAggregationPredicates {} -> throw400 NotExists "Aggregation Predicates cannot appear in permission checks"
  AVRemoteRelationship {} -> throw400 NotExists "Remote relationships permission checks not implemented yet"

convAnnBoolExpPartialSQL ::
  (Applicative f) =>
  SessionVariableBuilder f ->
  AnnBoolExpPartialSQL ('Postgres 'Vanilla) ->
  f (AnnBoolExpSQL ('Postgres 'Vanilla))
convAnnBoolExpPartialSQL f =
  (traverse . traverse) (convPartialSQLExp f)

convAnnRedactionExpPartialSQL ::
  (Applicative f) =>
  SessionVariableBuilder f ->
  AnnRedactionExpPartialSQL ('Postgres 'Vanilla) ->
  f (AnnRedactionExp ('Postgres 'Vanilla) (SQLExpression ('Postgres 'Vanilla)))
convAnnRedactionExpPartialSQL f =
  traverse (convPartialSQLExp f)

convPartialSQLExp ::
  (Applicative f) =>
  SessionVariableBuilder f ->
  PartialSQLExp ('Postgres 'Vanilla) ->
  f (SQLExpression ('Postgres 'Vanilla))
convPartialSQLExp sessVarBldr = \case
  PSESQLExp sqlExp -> pure sqlExp
  PSESession -> pure $ _svbCurrentSession sessVarBldr
  PSESessVar colTy sessionVariable -> (_svbVariableParser sessVarBldr) colTy sessionVariable

sessVarFromCurrentSetting ::
  (Applicative f) => SessionVariableBuilder f
sessVarFromCurrentSetting =
  SessionVariableBuilder currentSession $ \ty var -> pure $ sessVarFromCurrentSetting' ty var

sessVarFromCurrentSetting' :: CollectableType PGScalarType -> SessionVariable -> S.SQLExp
sessVarFromCurrentSetting' ty sessVar =
  withTypeAnn ty $ fromCurrentSession currentSession sessVar

fromCurrentSession ::
  S.SQLExp ->
  SessionVariable ->
  S.SQLExp
fromCurrentSession currentSessionExp sessVar =
  S.SEOpApp
    (S.SQLOp "->>")
    [currentSessionExp, S.SELit $ sessionVariableToText sessVar]

currentSession :: S.SQLExp
currentSession = S.SEUnsafe "current_setting('hasura.user')::json"

checkSelPerm ::
  (UserInfoM m, QErrM m, TableInfoRM ('Postgres 'Vanilla) m) =>
  SelPermInfo ('Postgres 'Vanilla) ->
  SessionVariableBuilder m ->
  AnnBoolExpSQL ('Postgres 'Vanilla) ->
  m (AnnBoolExpSQL ('Postgres 'Vanilla))
checkSelPerm spi sessVarBldr =
  traverse (checkOnColExp spi sessVarBldr)

convBoolExp ::
  (UserInfoM m, QErrM m, TableInfoRM ('Postgres 'Vanilla) m, LogicalModelFieldsRM ('Postgres 'Vanilla) m) =>
  FieldInfoMap (FieldInfo ('Postgres 'Vanilla)) ->
  SelPermInfo ('Postgres 'Vanilla) ->
  BoolExp ('Postgres 'Vanilla) ->
  SessionVariableBuilder m ->
  FieldInfoMap (FieldInfo ('Postgres 'Vanilla)) ->
  ValueParser ('Postgres 'Vanilla) m (SQLExpression ('Postgres 'Vanilla)) ->
  m (AnnBoolExpSQL ('Postgres 'Vanilla))
convBoolExp cim spi be sessVarBldr rootFieldInfoMap rhsParser = do
  let boolExpRHSParser = BoolExpRHSParser rhsParser $ _svbCurrentSession sessVarBldr
  abe <- annBoolExp boolExpRHSParser rootFieldInfoMap cim $ unBoolExp be
  checkSelPerm spi sessVarBldr abe

-- validate headers
validateHeaders :: (UserInfoM m, QErrM m) => HashSet Text -> m ()
validateHeaders depHeaders = do
  headers <- getSessionVariables . _uiSession <$> askUserInfo
  forM_ depHeaders $ \hdr ->
    unless (hdr `elem` map T.toLower headers)
      $ throw400 NotFound
      $ hdr
      <<> " header is expected but not found"

-- validate limit and offset int values
onlyPositiveInt :: (MonadError QErr m) => Int -> m ()
onlyPositiveInt i =
  when (i < 0)
    $ throw400
      NotSupported
      "unexpected negative value"
