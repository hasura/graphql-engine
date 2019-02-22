module Hasura.RQL.DML.Internal where

import qualified Database.PG.Query            as Q
import qualified Database.PG.Query.Connection as Q
import qualified Hasura.SQL.DML               as S

import           Hasura.Prelude
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import           Control.Lens
import           Data.Aeson.Types

import qualified Data.HashMap.Strict          as M
import qualified Data.HashSet                 as HS
import qualified Data.Sequence                as DS
import qualified Data.Text                    as T

newtype DMLP1 a
  = DMLP1 {unDMLP1 :: StateT (DS.Seq Q.PrepArg) P1 a}
  deriving ( Functor, Applicative
           , Monad
           , MonadState (DS.Seq Q.PrepArg)
           , MonadError QErr
           )

liftDMLP1
  :: (QErrM m, UserInfoM m, CacheRM m)
  => DMLP1 a -> m (a, DS.Seq Q.PrepArg)
liftDMLP1 =
  liftP1 . flip runStateT DS.empty . unDMLP1

instance CacheRM DMLP1 where
  askSchemaCache = DMLP1 $ lift askSchemaCache

instance UserInfoM DMLP1 where
  askUserInfo = DMLP1 $ lift askUserInfo

mkAdminRolePermInfo :: TableInfo -> RolePermInfo
mkAdminRolePermInfo ti =
  RolePermInfo (Just i) (Just s) (Just u) (Just d)
  where
    pgCols = map pgiName
      . fst . partitionEithers
      . map fieldInfoToEither . M.elems $ tiFieldInfoMap ti

    tn = tiName ti
    i = InsPermInfo tn annBoolExpTrue M.empty []
    s = SelPermInfo (HS.fromList pgCols) tn annBoolExpTrue
        Nothing True []
    u = UpdPermInfo (HS.fromList pgCols) tn annBoolExpTrue M.empty []
    d = DelPermInfo tn annBoolExpTrue []

askPermInfo'
  :: (UserInfoM m)
  => PermAccessor c
  -> TableInfo
  -> m (Maybe c)
askPermInfo' pa tableInfo = do
  roleName <- askCurRole
  let mrpi = getRolePermInfo roleName
  return $ mrpi >>= (^. permAccToLens pa)
  where
    rpim = tiRolePermInfoMap tableInfo
    getRolePermInfo roleName
      | roleName == adminRole = Just $ mkAdminRolePermInfo tableInfo
      | otherwise             = M.lookup roleName rpim

askPermInfo
  :: (UserInfoM m, QErrM m)
  => PermAccessor c
  -> TableInfo
  -> m c
askPermInfo pa tableInfo = do
  roleName <- askCurRole
  mPermInfo <- askPermInfo' pa tableInfo
  case mPermInfo of
    Just c  -> return c
    Nothing -> throw400 PermissionDenied $ mconcat
      [ pt <> " on " <>> tiName tableInfo
      , " for role " <>> roleName
      , " is not allowed. "
      ]
  where
    pt = permTypeToCode $ permAccToType pa

isTabUpdatable :: RoleName -> TableInfo -> Bool
isTabUpdatable role ti
  | role == adminRole = True
  | otherwise = isJust $ M.lookup role rpim >>= _permUpd
  where
    rpim = tiRolePermInfoMap ti

askInsPermInfo
  :: (UserInfoM m, QErrM m)
  => TableInfo -> m InsPermInfo
askInsPermInfo = askPermInfo PAInsert

askSelPermInfo
  :: (UserInfoM m, QErrM m)
  => TableInfo -> m SelPermInfo
askSelPermInfo = askPermInfo PASelect

askUpdPermInfo
  :: (UserInfoM m, QErrM m)
  => TableInfo -> m UpdPermInfo
askUpdPermInfo = askPermInfo PAUpdate

askDelPermInfo
  :: (UserInfoM m, QErrM m)
  => TableInfo -> m DelPermInfo
askDelPermInfo = askPermInfo PADelete

verifyAsrns :: (MonadError QErr m) => [a -> m ()] -> [a] -> m ()
verifyAsrns preds xs = indexedForM_ xs $ \a -> mapM_ ($ a) preds

checkSelOnCol :: (UserInfoM m, QErrM m)
              => SelPermInfo -> PGCol -> m ()
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
    permErrMsg (RoleName "admin") =
      "no such column exists : " <>> pgCol
    permErrMsg roleName =
      mconcat
      [ "role " <>> roleName
      , " does not have permission to "
      , permTypeToCode pt <> " column " <>> pgCol
      ]

binRHSBuilder
  :: PGColType -> Value -> DMLP1 S.SQLExp
binRHSBuilder colType val = do
  preparedArgs <- get
  binVal <- runAesonParser (convToBin colType) val
  put (preparedArgs DS.|> binVal)
  return $ toPrepParam (DS.length preparedArgs + 1) colType

fetchRelTabInfo
  :: (QErrM m, CacheRM m)
  => QualifiedTable
  -> m TableInfo
fetchRelTabInfo refTabName =
  -- Internal error
  modifyErrAndSet500 ("foreign " <> ) $ askTabInfo refTabName

fetchRelDet
  :: (UserInfoM m, QErrM m, CacheRM m)
  => RelName -> QualifiedTable
  -> m (FieldInfoMap, SelPermInfo)
fetchRelDet relName refTabName = do
  roleName <- askCurRole
  -- Internal error
  refTabInfo <- fetchRelTabInfo refTabName
  -- Get the correct constraint that applies to the given relationship
  refSelPerm <- modifyErr (relPermErr refTabName roleName) $
                askSelPermInfo refTabInfo

  return (tiFieldInfoMap refTabInfo, refSelPerm)
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
  => SelPermInfo
  -> AnnBoolExpFldSQL
  -> m AnnBoolExpFldSQL
checkOnColExp spi annFld = case annFld of
  AVCol (PGColInfo cn _ _) _ -> do
    checkSelOnCol spi cn
    return annFld
  AVRel relInfo nesAnn -> do
    relSPI <- snd <$> fetchRelDet (riName relInfo) (riRTable relInfo)
    modAnn <- checkSelPerm relSPI nesAnn
    return $ AVRel relInfo $
      andAnnBoolExps modAnn $ spiFilter relSPI

checkSelPerm
  :: (UserInfoM m, QErrM m, CacheRM m)
  => SelPermInfo
  -> AnnBoolExpSQL
  -> m AnnBoolExpSQL
checkSelPerm spi =
  traverse (checkOnColExp spi)

convBoolExp'
  :: ( UserInfoM m, QErrM m, CacheRM m)
  => FieldInfoMap
  -> SelPermInfo
  -> BoolExp
  -> (PGColType -> Value -> m S.SQLExp)
  -> m AnnBoolExpSQL
convBoolExp' cim spi be prepValBuilder = do
  abe <- annBoolExp prepValBuilder cim be
  checkSelPerm spi abe

dmlTxErrorHandler :: Q.PGTxErr -> QErr
dmlTxErrorHandler p2Res =
  case err of
    Nothing          -> defaultTxErrorHandler p2Res
    Just (code, msg) -> err400 code msg
  where err = simplifyError p2Res

toJSONableExp :: PGColType -> S.SQLExp -> S.SQLExp
toJSONableExp colTy expn
  | colTy == PGGeometry || colTy == PGGeography =
      S.SEFnApp "ST_AsGeoJSON"
      [ expn
      , S.SEUnsafe "15" -- max decimal digits
      , S.SEUnsafe "4"  -- to print out crs
      ] Nothing
      `S.SETyAnn` S.jsonType
  | colTy == PGBigInt || colTy == PGBigSerial =
      expn `S.SETyAnn` S.textType
  | otherwise = expn

-- validate headers
validateHeaders :: (UserInfoM m, QErrM m) => [T.Text] -> m ()
validateHeaders depHeaders = do
  headers <- getVarNames . userVars <$> askUserInfo
  forM_ depHeaders $ \hdr ->
    unless (hdr `elem` map T.toLower headers) $
    throw400 NotFound $ hdr <<> " header is expected but not found"

simplifyError :: Q.PGTxErr -> Maybe (Code, T.Text)
simplifyError txErr = do
  stmtErr <- Q.getPGStmtErr txErr
  codeMsg <- getPGCodeMsg stmtErr
  extractError codeMsg
  where
    getPGCodeMsg pged =
      (,) <$> Q.edStatusCode pged <*> Q.edMessage pged
    extractError = \case
      -- restrict violation
      ("23001", msg) ->
        return (ConstraintViolation, "Can not delete or update due to data being referred. " <> msg)
      -- not null violation
      ("23502", msg) ->
        return (ConstraintViolation, "Not-NULL violation. " <> msg)
      -- foreign key violation
      ("23503", msg) ->
        return  (ConstraintViolation, "Foreign key violation. " <> msg)
      -- unique violation
      ("23505", msg) ->
        return  (ConstraintViolation, "Uniqueness violation. " <> msg)
      -- check violation
      ("23514", msg) ->
        return (PermissionError, "Check constraint violation. " <> msg)
      -- invalid text representation
      ("22P02", msg) -> return (DataException, msg)
      -- invalid parameter value
      ("22023", msg) -> return (DataException, msg)
      -- no unique constraint on the columns
      ("42P10", _)   ->
        return (ConstraintError, "there is no unique or exclusion constraint on target column(s)")
      -- no constraint
      ("42704", msg) -> return (ConstraintError, msg)
      -- invalid input values
      ("22007", msg) -> return (DataException, msg)
      _              -> Nothing

-- validate limit and offset int values
onlyPositiveInt :: MonadError QErr m => Int -> m ()
onlyPositiveInt i = when (i < 0) $ throw400 NotSupported
  "unexpected negative value"
