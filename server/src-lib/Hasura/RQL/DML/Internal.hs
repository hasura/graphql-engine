module Hasura.RQL.DML.Internal where
-- ( mkAdminRolePermInfo
-- , SessVarBldr
-- ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                        as M
import qualified Data.HashSet                               as HS
import qualified Data.Sequence                              as DS
import qualified Data.Text                                  as T
import qualified Database.PG.Query                          as Q

import           Control.Lens
import           Data.Aeson.Types
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.SQL.DML           as S

import           Hasura.Backends.Postgres.SQL.Error
import           Hasura.Backends.Postgres.SQL.Types         hiding (TableName)
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.Backends.Postgres.Translate.BoolExp
import           Hasura.Backends.Postgres.Translate.Column
import           Hasura.Backends.Postgres.Types.Column
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.Session


newtype DMLP1T m a
  = DMLP1T { unDMLP1T :: StateT (DS.Seq Q.PrepArg) m a }
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadState (DS.Seq Q.PrepArg), MonadError e
           , SourceM, TableCoreInfoRM b, TableInfoRM b, CacheRM, UserInfoM, HasServerConfigCtx
           )

runDMLP1T :: DMLP1T m a -> m (a, DS.Seq Q.PrepArg)
runDMLP1T = flip runStateT DS.empty . unDMLP1T

mkAdminRolePermInfo :: Backend b => TableCoreInfo b -> RolePermInfo b
mkAdminRolePermInfo ti =
  RolePermInfo (Just i) (Just s) (Just u) (Just d)
  where
    fields = _tciFieldInfoMap ti
    pgCols = map pgiColumn $ getCols fields
    pgColsWithFilter = M.fromList $ map (, Nothing) pgCols
    scalarComputedFields =
      HS.fromList $ map _cfiName $ onlyScalarComputedFields $ getComputedFieldInfos fields
    scalarComputedFields' = HS.toMap scalarComputedFields $> Nothing


    tn = _tciName ti
    i = InsPermInfo (HS.fromList pgCols) annBoolExpTrue M.empty False []
    s = SelPermInfo pgColsWithFilter scalarComputedFields' annBoolExpTrue Nothing True []
    u = UpdPermInfo (HS.fromList pgCols) tn annBoolExpTrue Nothing M.empty []
    d = DelPermInfo tn annBoolExpTrue []

askPermInfo'
  :: (UserInfoM m, Backend b)
  => PermAccessor b c
  -> TableInfo b
  -> m (Maybe c)
askPermInfo' pa tableInfo = do
  role <- askCurRole
  return $ getPermInfoMaybe role pa tableInfo

getPermInfoMaybe
  :: (Backend b) => RoleName -> PermAccessor b c -> TableInfo b -> Maybe c
getPermInfoMaybe role pa tableInfo =
  getRolePermInfo role tableInfo >>= (^. permAccToLens pa)

getRolePermInfo
  :: Backend b => RoleName -> TableInfo b -> Maybe (RolePermInfo b)
getRolePermInfo role tableInfo
  | role == adminRoleName =
    Just $ mkAdminRolePermInfo (_tiCoreInfo tableInfo)
  | otherwise         =
    M.lookup role (_tiRolePermInfoMap tableInfo)

askPermInfo
  :: (UserInfoM m, QErrM m, Backend b)
  => PermAccessor b c
  -> TableInfo b
  -> m c
askPermInfo pa tableInfo = do
  roleName  <- askCurRole
  mPermInfo <- askPermInfo' pa tableInfo
  onNothing mPermInfo $ throw400 PermissionDenied $ mconcat
    [ pt <> " on " <>> _tciName (_tiCoreInfo tableInfo)
    , " for role " <>> roleName
    , " is not allowed. "
    ]
  where
    pt = permTypeToCode $ permAccToType pa

isTabUpdatable :: RoleName -> TableInfo 'Postgres -> Bool
isTabUpdatable role ti
  | role == adminRoleName = True
  | otherwise = isJust $ M.lookup role rpim >>= _permUpd
  where
    rpim = _tiRolePermInfoMap ti

askInsPermInfo
  :: (UserInfoM m, QErrM m, Backend b)
  => TableInfo b -> m (InsPermInfo b)
askInsPermInfo = askPermInfo PAInsert

askSelPermInfo
  :: (UserInfoM m, QErrM m, Backend b)
  => TableInfo b -> m (SelPermInfo b)
askSelPermInfo = askPermInfo PASelect

askUpdPermInfo
  :: (UserInfoM m, QErrM m, Backend b)
  => TableInfo b -> m (UpdPermInfo b)
askUpdPermInfo = askPermInfo PAUpdate

askDelPermInfo
  :: (UserInfoM m, QErrM m, Backend b)
  => TableInfo b -> m (DelPermInfo b)
askDelPermInfo = askPermInfo PADelete

verifyAsrns :: (MonadError QErr m) => [a -> m ()] -> [a] -> m ()
verifyAsrns preds xs = indexedForM_ xs $ \a -> mapM_ ($ a) preds

checkSelOnCol :: forall m b. (UserInfoM m, QErrM m, Backend b)
              => SelPermInfo b -> Column b -> m ()
checkSelOnCol selPermInfo =
  checkPermOnCol PTSelect (HS.fromList $ M.keys $ spiCols selPermInfo)

checkPermOnCol
  :: (UserInfoM m, QErrM m, Backend b)
  => PermType
  -> HS.HashSet (Column b)
  -> Column b
  -> m ()
checkPermOnCol pt allowedCols col = do
  role <- askCurRole
  unless (HS.member col allowedCols) $
    throw400 PermissionDenied $ permErrMsg role
  where
    permErrMsg role
      | role == adminRoleName = "no such column exists : " <>> col
      | otherwise = mconcat
        [ "role " <>> role
        , " does not have permission to "
        , permTypeToCode pt <> " column " <>> col
        ]

valueParserWithCollectableType
  :: (MonadError QErr m)
  => (ColumnType 'Postgres -> Value -> m S.SQLExp)
  -> CollectableType (ColumnType 'Postgres)
  -> Value
  -> m S.SQLExp
valueParserWithCollectableType valBldr pgType val = case pgType of
  CollectableTypeScalar ty  -> valBldr ty val
  CollectableTypeArray ofTy -> do
    -- for arrays, we don't use the prepared builder
    vals <- runAesonParser parseJSON val
    scalarValues <- parseScalarValuesColumnType ofTy vals
    return $ S.SETyAnn
      (S.SEArray $ map (toTxtValue . ColumnValue ofTy) scalarValues)
      (S.mkTypeAnn $ CollectableTypeArray (unsafePGColumnToBackend ofTy))

binRHSBuilder :: (QErrM m)
  => ColumnType 'Postgres -> Value -> DMLP1T m S.SQLExp
binRHSBuilder colType val = do
  preparedArgs <- get
  scalarValue <- parseScalarValueColumnType colType val
  put (preparedArgs DS.|> binEncoder scalarValue)
  return $ toPrepParam (DS.length preparedArgs + 1) (unsafePGColumnToBackend colType)

fetchRelTabInfo
  :: (QErrM m, TableInfoRM b m, Backend b)
  => TableName b -> m (TableInfo b)
fetchRelTabInfo refTabName =
  -- Internal error
  modifyErrAndSet500 ("foreign " <> ) $
    askTabInfoSource refTabName

type SessVarBldr b m = SessionVarType b -> SessionVariable -> m (SQLExpression b)

fetchRelDet
  :: (UserInfoM m, QErrM m, TableInfoRM b m, Backend b)
  => RelName -> TableName b
  -> m (FieldInfoMap (FieldInfo b), SelPermInfo b)
fetchRelDet relName refTabName = do
  roleName <- askCurRole
  -- Internal error
  refTabInfo <- fetchRelTabInfo refTabName
  -- Get the correct constraint that applies to the given relationship
  refSelPerm <- modifyErr (relPermErr refTabName roleName) $
                askSelPermInfo refTabInfo

  return (_tciFieldInfoMap $ _tiCoreInfo refTabInfo, refSelPerm)
  where
    relPermErr rTable roleName _ =
      mconcat
      [ "role " <>> roleName
      , " does not have permission to read relationship " <>> relName
      , "; no permission on"
      , " table " <>> rTable
      ]

checkOnColExp
  :: (UserInfoM m, QErrM m, TableInfoRM b m, Backend b)
  => SelPermInfo b
  -> SessVarBldr b m
  -> AnnBoolExpFldSQL b
  -> m (AnnBoolExpFldSQL b)
checkOnColExp spi sessVarBldr annFld = case annFld of
  AVCol colInfo _ -> do
    let cn = pgiColumn colInfo
    checkSelOnCol spi cn
    return annFld
  AVRel relInfo nesAnn -> do
    relSPI <- snd <$> fetchRelDet (riName relInfo) (riRTable relInfo)
    modAnn <- checkSelPerm relSPI sessVarBldr nesAnn
    resolvedFltr <- convAnnBoolExpPartialSQL sessVarBldr $ spiFilter relSPI
    return $ AVRel relInfo $ andAnnBoolExps modAnn resolvedFltr

convAnnBoolExpPartialSQL
  :: (Applicative f, Backend backend)
  => SessVarBldr backend f
  -> AnnBoolExpPartialSQL backend
  -> f (AnnBoolExpSQL backend)
convAnnBoolExpPartialSQL f =
  traverseAnnBoolExp (convPartialSQLExp f)

convAnnColumnCaseBoolExpPartialSQL
  :: (Applicative f, Backend backend)
  => SessVarBldr backend f
  -> AnnColumnCaseBoolExpPartialSQL backend
  -> f (AnnColumnCaseBoolExp backend (SQLExpression backend))
convAnnColumnCaseBoolExpPartialSQL f =
  traverseAnnColumnCaseBoolExp (convPartialSQLExp f)

convPartialSQLExp
  :: (Applicative f)
  => SessVarBldr backend f
  -> PartialSQLExp backend
  -> f (SQLExpression backend)
convPartialSQLExp f = \case
  PSESQLExp sqlExp                 -> pure sqlExp
  PSESessVar colTy sessionVariable -> f colTy sessionVariable

sessVarFromCurrentSetting
  :: (Applicative f) => CollectableType PGScalarType -> SessionVariable -> f S.SQLExp
sessVarFromCurrentSetting pgType sessVar =
  pure $ sessVarFromCurrentSetting' pgType sessVar

sessVarFromCurrentSetting' :: CollectableType PGScalarType -> SessionVariable -> S.SQLExp
sessVarFromCurrentSetting' ty sessVar =
  withTypeAnn ty $ fromCurrentSession currentSession sessVar

withTypeAnn :: CollectableType PGScalarType -> S.SQLExp -> S.SQLExp
withTypeAnn ty sessVarVal = flip S.SETyAnn (S.mkTypeAnn ty) $
  case ty of
    CollectableTypeScalar baseTy -> withConstructorFn baseTy sessVarVal
    CollectableTypeArray _       -> sessVarVal

retrieveAndFlagSessionVariableValue
  :: (MonadState s m)
  => (SessionVariable -> s -> s)
  -> SessionVariable
  -> S.SQLExp
  -> m S.SQLExp
retrieveAndFlagSessionVariableValue updateState sessVar currentSessionExp = do
  modify $ updateState sessVar
  pure $ fromCurrentSession currentSessionExp sessVar

fromCurrentSession
  :: S.SQLExp
  -> SessionVariable
  -> S.SQLExp
fromCurrentSession currentSessionExp sessVar =
  S.SEOpApp (S.SQLOp "->>")
    [currentSessionExp, S.SELit $ sessionVariableToText sessVar]

currentSession :: S.SQLExp
currentSession = S.SEUnsafe "current_setting('hasura.user')::json"

checkSelPerm
  :: (UserInfoM m, QErrM m, TableInfoRM b m, Backend b)
  => SelPermInfo b
  -> SessVarBldr b m
  -> AnnBoolExpSQL b
  -> m (AnnBoolExpSQL b)
checkSelPerm spi sessVarBldr =
  traverse (checkOnColExp spi sessVarBldr)

convBoolExp
  :: (UserInfoM m, QErrM m, TableInfoRM b m, BackendMetadata b)
  => FieldInfoMap (FieldInfo b)
  -> SelPermInfo b
  -> BoolExp b
  -> SessVarBldr b m
  -> TableName b
  -> ValueParser b m (SQLExpression b)
  -> m (AnnBoolExpSQL b)
convBoolExp cim spi be sessVarBldr rootTable rhsParser = do
  abe <- annBoolExp rhsParser rootTable cim $ unBoolExp be
  checkSelPerm spi sessVarBldr abe

dmlTxErrorHandler :: Q.PGTxErr -> QErr
dmlTxErrorHandler = mkTxErrorHandler $ \case
  PGIntegrityConstraintViolation _ -> True
  PGDataException _ -> True
  PGSyntaxErrorOrAccessRuleViolation (Just (PGErrorSpecific code)) -> code `elem`
    [ PGUndefinedObject
    , PGInvalidColumnReference ]
  _ -> False

toJSONableExp :: Bool -> ColumnType 'Postgres -> Bool -> S.SQLExp -> S.SQLExp
toJSONableExp strfyNum colTy asText expn
  | asText || (isScalarColumnWhere isBigNum colTy && strfyNum) =
    expn `S.SETyAnn` S.textTypeAnn
  | isScalarColumnWhere isGeoType colTy =
      S.SEFnApp "ST_AsGeoJSON"
      [ expn
      , S.SEUnsafe "15" -- max decimal digits
      , S.SEUnsafe "4"  -- to print out crs
      ] Nothing
      `S.SETyAnn` S.jsonTypeAnn
  | otherwise = expn

-- validate headers
validateHeaders :: (UserInfoM m, QErrM m) => [Text] -> m ()
validateHeaders depHeaders = do
  headers <- getSessionVariables . _uiSession <$> askUserInfo
  forM_ depHeaders $ \hdr ->
    unless (hdr `elem` map T.toLower headers) $
    throw400 NotFound $ hdr <<> " header is expected but not found"

-- validate limit and offset int values
onlyPositiveInt :: MonadError QErr m => Int -> m ()
onlyPositiveInt i = when (i < 0) $ throw400 NotSupported
  "unexpected negative value"
