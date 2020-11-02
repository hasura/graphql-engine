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
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.Backends.Postgres.Translate.BoolExp
import           Hasura.RQL.Types
import           Hasura.Session


newtype DMLP1T m a
  = DMLP1T { unDMLP1T :: StateT (DS.Seq Q.PrepArg) m a }
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadState (DS.Seq Q.PrepArg), MonadError e
           , TableCoreInfoRM, CacheRM, UserInfoM, HasSQLGenCtx
           )

runDMLP1T :: DMLP1T m a -> m (a, DS.Seq Q.PrepArg)
runDMLP1T = flip runStateT DS.empty . unDMLP1T

mkAdminRolePermInfo :: (Eq (Column backend), Hashable (Column backend)) => TableCoreInfo backend -> RolePermInfo backend
mkAdminRolePermInfo ti =
  RolePermInfo (Just i) (Just s) (Just u) (Just d)
  where
    fields = _tciFieldInfoMap ti
    pgCols = map pgiColumn $ getCols fields
    scalarComputedFields = map _cfiName $ onlyScalarComputedFields $
                     getComputedFieldInfos fields

    tn = _tciName ti
    i = InsPermInfo (HS.fromList pgCols) annBoolExpTrue M.empty False []
    s = SelPermInfo (HS.fromList pgCols) (HS.fromList scalarComputedFields) annBoolExpTrue
        Nothing True []
    u = UpdPermInfo (HS.fromList pgCols) tn annBoolExpTrue Nothing M.empty []
    d = DelPermInfo tn annBoolExpTrue []

askPermInfo'
  :: (UserInfoM m)
  => PermAccessor 'Postgres c
  -> TableInfo 'Postgres
  -> m (Maybe c)
askPermInfo' pa tableInfo = do
  roleName <- askCurRole
  return $ getPermInfoMaybe roleName pa tableInfo

getPermInfoMaybe :: RoleName -> PermAccessor 'Postgres c -> TableInfo 'Postgres -> Maybe c
getPermInfoMaybe roleName pa tableInfo =
  getRolePermInfo roleName tableInfo >>= (^. permAccToLens pa)

getRolePermInfo :: RoleName -> TableInfo 'Postgres -> Maybe (RolePermInfo 'Postgres)
getRolePermInfo roleName tableInfo
  | roleName == adminRoleName =
    Just $ mkAdminRolePermInfo (_tiCoreInfo tableInfo)
  | otherwise                 =
    M.lookup roleName (_tiRolePermInfoMap tableInfo)

askPermInfo
  :: (UserInfoM m, QErrM m)
  => PermAccessor 'Postgres c
  -> TableInfo 'Postgres
  -> m c
askPermInfo pa tableInfo = do
  roleName <- askCurRole
  mPermInfo <- askPermInfo' pa tableInfo
  case mPermInfo of
    Just c  -> return c
    Nothing -> throw400 PermissionDenied $ mconcat
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
  :: (UserInfoM m, QErrM m)
  => TableInfo 'Postgres -> m (InsPermInfo 'Postgres)
askInsPermInfo = askPermInfo PAInsert

askSelPermInfo
  :: (UserInfoM m, QErrM m)
  => TableInfo 'Postgres -> m (SelPermInfo 'Postgres)
askSelPermInfo = askPermInfo PASelect

askUpdPermInfo
  :: (UserInfoM m, QErrM m)
  => TableInfo 'Postgres -> m (UpdPermInfo 'Postgres)
askUpdPermInfo = askPermInfo PAUpdate

askDelPermInfo
  :: (UserInfoM m, QErrM m)
  => TableInfo 'Postgres -> m (DelPermInfo 'Postgres)
askDelPermInfo = askPermInfo PADelete

verifyAsrns :: (MonadError QErr m) => [a -> m ()] -> [a] -> m ()
verifyAsrns preds xs = indexedForM_ xs $ \a -> mapM_ ($ a) preds

checkSelOnCol :: (UserInfoM m, QErrM m)
              => SelPermInfo 'Postgres -> PGCol -> m ()
checkSelOnCol selPermInfo =
  checkPermOnCol PTSelect (spiCols selPermInfo)

checkPermOnCol
  :: (UserInfoM m, QErrM m)
  => PermType
  -> HS.HashSet PGCol
  -> PGCol
  -> m ()
checkPermOnCol pt allowedCols pgCol = do
  roleName <- askCurRole
  unless (HS.member pgCol allowedCols) $
    throw400 PermissionDenied $ permErrMsg roleName
  where
    permErrMsg roleName
      | roleName == adminRoleName = "no such column exists : " <>> pgCol
      | otherwise = mconcat
        [ "role " <>> roleName
        , " does not have permission to "
        , permTypeToCode pt <> " column " <>> pgCol
        ]

binRHSBuilder :: (QErrM m) => PGColumnType -> Value -> DMLP1T m S.SQLExp
binRHSBuilder colType val = do
  preparedArgs <- get
  scalarValue <- parsePGScalarValue colType val
  put (preparedArgs DS.|> toBinaryValue scalarValue)
  return $ toPrepParam (DS.length preparedArgs + 1) (pstType scalarValue)

fetchRelTabInfo
  :: (QErrM m, CacheRM m)
  => QualifiedTable
  -> m (TableInfo 'Postgres)
fetchRelTabInfo refTabName =
  -- Internal error
  modifyErrAndSet500 ("foreign " <> ) $ askTabInfo refTabName

type SessVarBldr b m = PGType (ScalarType b) -> SessionVariable -> m S.SQLExp

fetchRelDet
  :: (UserInfoM m, QErrM m, CacheRM m)
  => RelName -> QualifiedTable
  -> m (FieldInfoMap (FieldInfo 'Postgres), SelPermInfo 'Postgres)
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
  :: (UserInfoM m, QErrM m, CacheRM m)
  => SelPermInfo 'Postgres
  -> SessVarBldr 'Postgres m
  -> AnnBoolExpFldSQL 'Postgres
  -> m (AnnBoolExpFldSQL 'Postgres)
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
  :: (Applicative f)
  => SessVarBldr backend f
  -> AnnBoolExpPartialSQL backend
  -> f (AnnBoolExpSQL backend)
convAnnBoolExpPartialSQL f =
  traverseAnnBoolExp (convPartialSQLExp f)

convPartialSQLExp
  :: (Applicative f)
  => SessVarBldr backend f
  -> PartialSQLExp backend
  -> f S.SQLExp
convPartialSQLExp f = \case
  PSESQLExp sqlExp                 -> pure sqlExp
  PSESessVar colTy sessionVariable -> f colTy sessionVariable

sessVarFromCurrentSetting
  :: (Applicative f) => PGType PGScalarType -> SessionVariable -> f S.SQLExp
sessVarFromCurrentSetting pgType sessVar =
  pure $ sessVarFromCurrentSetting' pgType sessVar

sessVarFromCurrentSetting' :: PGType PGScalarType -> SessionVariable -> S.SQLExp
sessVarFromCurrentSetting' ty sessVar =
  flip S.SETyAnn (S.mkTypeAnn ty) $
  case ty of
    PGTypeScalar baseTy -> withConstructorFn baseTy sessVarVal
    PGTypeArray _       -> sessVarVal
  where
    sessVarVal = S.SEOpApp (S.SQLOp "->>")
                 [currentSession, S.SELit $ sessionVariableToText sessVar]

currentSession :: S.SQLExp
currentSession = S.SEUnsafe "current_setting('hasura.user')::json"

checkSelPerm
  :: (UserInfoM m, QErrM m, CacheRM m)
  => SelPermInfo 'Postgres
  -> SessVarBldr 'Postgres m
  -> AnnBoolExpSQL 'Postgres
  -> m (AnnBoolExpSQL 'Postgres)
checkSelPerm spi sessVarBldr =
  traverse (checkOnColExp spi sessVarBldr)

convBoolExp
  :: (UserInfoM m, QErrM m, CacheRM m)
  => FieldInfoMap (FieldInfo 'Postgres)
  -> SelPermInfo 'Postgres
  -> BoolExp 'Postgres
  -> SessVarBldr 'Postgres m
  -> (PGColumnType -> Value -> m S.SQLExp)
  -> m (AnnBoolExpSQL 'Postgres)
convBoolExp cim spi be sessVarBldr prepValBldr = do
  abe <- annBoolExp rhsParser cim $ unBoolExp be
  checkSelPerm spi sessVarBldr abe
  where
    rhsParser pgType val = case pgType of
      PGTypeScalar ty  -> prepValBldr ty val
      PGTypeArray ofTy -> do
        -- for arrays, we don't use the prepared builder
        vals <- runAesonParser parseJSON val
        WithScalarType scalarType scalarValues <- parsePGScalarValues ofTy vals
        return $ S.SETyAnn
          (S.SEArray $ map (toTxtValue . WithScalarType scalarType) scalarValues)
          (S.mkTypeAnn $ PGTypeArray scalarType)

dmlTxErrorHandler :: Q.PGTxErr -> QErr
dmlTxErrorHandler = mkTxErrorHandler $ \case
  PGIntegrityConstraintViolation _ -> True
  PGDataException _ -> True
  PGSyntaxErrorOrAccessRuleViolation (Just (PGErrorSpecific code)) -> code `elem`
    [ PGUndefinedObject
    , PGInvalidColumnReference ]
  _ -> False

toJSONableExp :: Bool -> PGColumnType -> Bool -> S.SQLExp -> S.SQLExp
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
