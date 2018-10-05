{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Hasura.RQL.DML.Internal where

import qualified Database.PG.Query            as Q
import qualified Database.PG.Query.Connection as Q
import qualified Hasura.SQL.DML               as S

import           Hasura.SQL.Types
import           Hasura.SQL.Value
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.Prelude

import           Control.Lens
import           Data.Aeson.Types

import qualified Data.HashMap.Strict          as M
import qualified Data.HashSet                 as HS
import qualified Data.Sequence                as DS
import qualified Data.Text                    as T

-- class (P1C m) => Preparable m where
--   prepValBuilder :: PGColType -> Value -> m S.SQLExp

type DMLP1 = StateT (DS.Seq Q.PrepArg) P1

instance CacheRM DMLP1 where
  askSchemaCache = lift askSchemaCache

instance UserInfoM DMLP1 where
  askUserInfo = lift askUserInfo

-- instance P1C DMLP1 where
--   askUserInfo = lift askUserInfo

-- instance Preparable DMLP1 where
--   prepValBuilder = binRHSBuilder

peelDMLP1 :: QCtx -> DMLP1 a -> Either QErr (a, [Q.PrepArg])
peelDMLP1 qEnv m = do
  (a, prepSeq) <- runP1 qEnv $ runStateT m DS.empty
  return (a, toList prepSeq)

mkAdminRolePermInfo :: TableInfo -> RolePermInfo
mkAdminRolePermInfo ti =
  RolePermInfo (Just i) (Just s) (Just u) (Just d)
  where
    pgCols = map pgiName
      . fst . partitionEithers
      . map fieldInfoToEither . M.elems $ tiFieldInfoMap ti

    tn = tiName ti
    i = InsPermInfo tn (S.BELit True) True [] []
    s = SelPermInfo (HS.fromList pgCols) tn (S.BELit True) Nothing [] []
    u = UpdPermInfo (HS.fromList pgCols) tn (S.BELit True) [] []
    d = DelPermInfo tn (S.BELit True) [] []

askPermInfo'
  :: (P1C m)
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
  :: (P1C m)
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

askInsPermInfo
  :: (P1C m)
  => TableInfo -> m InsPermInfo
askInsPermInfo = askPermInfo PAInsert

askSelPermInfo
  :: (P1C m)
  => TableInfo -> m SelPermInfo
askSelPermInfo = askPermInfo PASelect

askUpdPermInfo
  :: (P1C m)
  => TableInfo -> m UpdPermInfo
askUpdPermInfo = askPermInfo PAUpdate

askDelPermInfo
  :: (P1C m)
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

binRHSBuilder :: PGColType -> Value -> DMLP1 S.SQLExp
binRHSBuilder colType val = do
  preparedArgs <- get
  binVal <- runAesonParser (convToBin colType) val
  put (preparedArgs DS.|> binVal)
  return $ toPrepParam (DS.length preparedArgs + 1) colType

fetchRelTabInfo
  :: (P1C m)
  => QualifiedTable
  -> m TableInfo
fetchRelTabInfo refTabName =
  -- Internal error
  modifyErrAndSet500 ("foreign " <> ) $ askTabInfo refTabName

fetchRelDet
  :: (P1C m)
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

checkOnColExp :: (P1C m)
              => SelPermInfo -> AnnValS -> m AnnValS
checkOnColExp spi annVal =
  case annVal of
    AVCol pci@(PGColInfo cn _ _) opExps -> do
      checkSelOnCol spi cn
      return $ AVCol pci opExps
    AVRel relInfo nesAnn _ -> do
      relSPI <- snd <$> fetchRelDet (riName relInfo) (riRTable relInfo)
      modAnn <- checkSelPerm relSPI nesAnn
      return $ AVRel relInfo modAnn $ spiFilter relSPI

checkSelPerm :: (P1C m)
             => SelPermInfo -> GBoolExp AnnValS -> m (GBoolExp AnnValS)
checkSelPerm spi = mapBoolExp (checkOnColExp spi)

convBoolExp
  :: (P1C m)
  => FieldInfoMap
  -> QualifiedTable
  -> SelPermInfo
  -> BoolExp
  -> (PGColType -> Value -> m S.SQLExp)
  -> m S.BoolExp
convBoolExp cim tn spi be prepValBuilder =
  cBoolExp <$> convBoolExp' cim tn spi be prepValBuilder

convBoolExp'
  :: (P1C m)
  => FieldInfoMap
  -> QualifiedTable
  -> SelPermInfo
  -> BoolExp
  -> (PGColType -> Value -> m S.SQLExp)
  -> m (GBoolExp AnnSQLBoolExp)
convBoolExp' cim tn spi be prepValBuilder = do
  abe <- annBoolExp prepValBuilder cim be
  modABE <- checkSelPerm spi abe
  convBoolRhs binStrat (S.mkQual tn) modABE
  where
    binStrat = mkBoolExpBuilder return

dmlTxErrorHandler :: Q.PGTxErr -> QErr
dmlTxErrorHandler p2Res =
  case err of
    Nothing  -> defaultTxErrorHandler p2Res
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
validateHeaders :: (P1C m) => [T.Text] -> m ()
validateHeaders depHeaders = do
  headers <- M.keys . userHeaders <$> askUserInfo
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
      _              -> Nothing

-- validate limit and offset int values
onlyPositiveInt :: MonadError QErr m => Int -> m ()
onlyPositiveInt i = when (i < 0) $ throw400 NotSupported
  "unexpected negative value"
