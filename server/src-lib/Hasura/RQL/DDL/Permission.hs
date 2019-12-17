module Hasura.RQL.DDL.Permission
    ( CreatePerm
    , runCreatePerm
    , purgePerm
    , PermDef(..)

    , InsPerm(..)
    , InsPermDef
    , CreateInsPerm
    , clearInsInfra
    , buildInsInfra
    , buildInsPermInfo
    , DropInsPerm
    , dropInsPermP2

    , SelPerm(..)
    , SelPermDef
    , CreateSelPerm
    , buildSelPermInfo
    , DropSelPerm
    , dropSelPermP2

    , UpdPerm(..)
    , UpdPermDef
    , CreateUpdPerm
    , buildUpdPermInfo
    , DropUpdPerm
    , dropUpdPermP2

    , DelPerm(..)
    , DelPermDef
    , CreateDelPerm
    , buildDelPermInfo
    , DropDelPerm
    , dropDelPermP2

    , IsPerm(..)
    , addPermP1
    , addPermP2

    , dropView
    , DropPerm
    , runDropPerm

    , SetPermComment(..)
    , runSetPermComment

    , rebuildPermInfo
    , fetchPermDef
    ) where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Permission.Triggers
import           Hasura.RQL.DML.Internal            hiding (askPermInfo)
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query                  as Q
import qualified Hasura.SQL.DML                     as S

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Crypto.Hash                        as CH
import qualified Data.HashMap.Strict                as HM
import qualified Data.HashSet                       as HS
import qualified Data.Text                          as T

-- Insert permission
data InsPerm
  = InsPerm
  { ipCheck   :: !BoolExp
  , ipSet     :: !(Maybe (ColumnValues Value))
  , ipColumns :: !(Maybe PermColSpec)
  } deriving (Show, Eq, Lift, Generic)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''InsPerm)

type InsPermDef = PermDef InsPerm
type CreateInsPerm = CreatePerm InsPerm

buildViewName :: QualifiedTable -> RoleName -> PermType -> QualifiedTable
buildViewName qt rn pt = QualifiedObject hdbViewsSchema tableName
  where
    -- Generate a unique hash for view name from role name, permission type and qualified table.
    -- See Note [Postgres identifier length limitations].
    -- Black2b_224 generates 56 character hash. See Note [Blake2b faster than SHA-256].
    -- Refer https://github.com/hasura/graphql-engine/issues/3444.
    tableName = TableName $ T.pack $ show hash
    hash :: CH.Digest CH.Blake2b_224 =
      CH.hash $ txtToBs $ roleNameToTxt rn <> "__" <> T.pack (show pt) <> "__" <> qualObjectToText qt

buildView :: QualifiedTable -> QualifiedTable -> Q.Query
buildView tn vn =
  Q.fromBuilder $ mconcat
  [ "CREATE VIEW " <> toSQL vn
  , " AS SELECT * FROM " <> toSQL tn
  ]

dropView :: QualifiedTable -> Q.Tx ()
dropView vn =
  Q.unitQ dropViewS () False
  where
    dropViewS = Q.fromBuilder $
      "DROP VIEW " <> toSQL vn

procSetObj
  :: (QErrM m)
  => TableInfo PGColumnInfo -> Maybe (ColumnValues Value)
  -> m (PreSetColsPartial, [Text], [SchemaDependency])
procSetObj ti mObj = do
  (setColTups, deps) <- withPathK "set" $
    fmap unzip $ forM (HM.toList setObj) $ \(pgCol, val) -> do
      ty <- askPGType fieldInfoMap pgCol $
        "column " <> pgCol <<> " not found in table " <>> tn
      sqlExp <- valueParser (PGTypeScalar ty) val
      let dep = mkColDep (getDepReason sqlExp) tn pgCol
      return ((pgCol, sqlExp), dep)
  return (HM.fromList setColTups, depHeaders, deps)
  where
    fieldInfoMap = _tiFieldInfoMap ti
    tn = _tiName ti
    setObj = fromMaybe mempty mObj
    depHeaders = getDepHeadersFromVal $ Object $
      HM.fromList $ map (first getPGColTxt) $ HM.toList setObj

    getDepReason = bool DRSessionVariable DROnType . isStaticValue

buildInsPermInfo
  :: (QErrM m, CacheRM m)
  => TableInfo PGColumnInfo
  -> PermDef InsPerm
  -> m (WithDeps InsPermInfo)
buildInsPermInfo tabInfo (PermDef rn (InsPerm chk set mCols) _) =
  withPathK "permission" $ do
  (be, beDeps) <- withPathK "check" $
    -- procBoolExp tn fieldInfoMap (S.QualVar "NEW") chk
    procBoolExp tn fieldInfoMap chk
  (setColsSQL, setHdrs, setColDeps) <- procSetObj tabInfo set
  void $ withPathK "columns" $ indexedForM insCols $ \col ->
         askPGType fieldInfoMap col ""
  let fltrHeaders = getDependentHeaders chk
      reqHdrs = fltrHeaders `union` setHdrs
      insColDeps = map (mkColDep DRUntyped tn) insCols
      deps = mkParentDep tn : beDeps ++ setColDeps ++ insColDeps
      insColsWithoutPresets = insCols \\ HM.keys setColsSQL
  return (InsPermInfo (HS.fromList insColsWithoutPresets) vn be setColsSQL reqHdrs, deps)
  where
    fieldInfoMap = _tiFieldInfoMap tabInfo
    tn = _tiName tabInfo
    vn = buildViewName tn rn PTInsert
    allCols = map pgiColumn $ getCols fieldInfoMap
    insCols = fromMaybe allCols $ convColSpec fieldInfoMap <$> mCols

buildInsInfra :: QualifiedTable -> InsPermInfo -> Q.TxE QErr ()
buildInsInfra tn (InsPermInfo _ vn be _ _) = do
  resolvedBoolExp <- convAnnBoolExpPartialSQL sessVarFromCurrentSetting be
  let trigFnQ = buildInsTrigFn vn tn $ toSQLBoolExp (S.QualVar "NEW") resolvedBoolExp
  Q.catchE defaultTxErrorHandler $ do
    -- Create the view
    Q.unitQ (buildView tn vn) () False
    -- Inject defaults on the view
    Q.discardQ (injectDefaults vn tn) () False
    -- Construct a trigger function
    Q.unitQ trigFnQ () False
    -- Add trigger for check expression
    Q.unitQ (buildInsTrig vn) () False

clearInsInfra :: QualifiedTable -> Q.TxE QErr ()
clearInsInfra vn =
  Q.catchE defaultTxErrorHandler $ do
    dropView vn
    Q.unitQ (dropInsTrigFn vn) () False

type DropInsPerm = DropPerm InsPerm

dropInsPermP2
  :: (CacheRWM m, MonadTx m)
  => DropInsPerm -> QualifiedTable -> m ()
dropInsPermP2 = dropPermP2

type instance PermInfo InsPerm = InsPermInfo

instance IsPerm InsPerm where

  type DropPermP1Res InsPerm = QualifiedTable

  permAccessor = PAInsert

  buildPermInfo = buildInsPermInfo

  addPermP2Setup qt _ = liftTx . buildInsInfra qt

  buildDropPermP1Res dp =
    ipiView <$> dropPermP1 dp

  dropPermP2Setup _ vn =
    liftTx $ clearInsInfra vn

-- Select constraint
data SelPerm
  = SelPerm
  { spColumns           :: !PermColSpec         -- ^ Allowed columns
  , spFilter            :: !BoolExp             -- ^ Filter expression
  , spLimit             :: !(Maybe Int)         -- ^ Limit value
  , spAllowAggregations :: !Bool                -- ^ Allow aggregation
  , spComputedFields    :: ![ComputedFieldName] -- ^ Allowed computed fields
  } deriving (Show, Eq, Lift, Generic)
$(deriveToJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''SelPerm)

instance FromJSON SelPerm where
  parseJSON = withObject "SelPerm" $ \o ->
    SelPerm
    <$> o .: "columns"
    <*> o .: "filter"
    <*> o .:? "limit"
    <*> o .:? "allow_aggregations" .!= False
    <*> o .:? "computed_fields" .!= []

buildSelPermInfo
  :: (QErrM m, CacheRM m)
  => TableInfo PGColumnInfo
  -> SelPerm
  -> m (WithDeps SelPermInfo)
buildSelPermInfo tabInfo sp = withPathK "permission" $ do
  let pgCols     = convColSpec fieldInfoMap $ spColumns sp

  (be, beDeps) <- withPathK "filter" $
    procBoolExp tn fieldInfoMap  $ spFilter sp

  -- check if the columns exist
  void $ withPathK "columns" $ indexedForM pgCols $ \pgCol ->
    askPGType fieldInfoMap pgCol autoInferredErr

  -- validate computed fields
  scalarComputedFields <-
    withPathK "computed_fields" $ indexedForM computedFields $ \fieldName -> do
      computedFieldInfo <- askComputedFieldInfo fieldInfoMap fieldName
      case _cfiReturnType computedFieldInfo of
        CFRScalar _               -> pure fieldName
        CFRSetofTable returnTable -> throw400 NotSupported $
          "select permissions on computed field " <> fieldName
          <<> " are auto-derived from the permissions on its returning table "
          <> returnTable <<> " and cannot be specified manually"

  let deps = mkParentDep tn : beDeps ++ map (mkColDep DRUntyped tn) pgCols
             ++ map (mkComputedFieldDep DRUntyped tn) scalarComputedFields
      depHeaders = getDependentHeaders $ spFilter sp
      mLimit = spLimit sp

  withPathK "limit" $ mapM_ onlyPositiveInt mLimit

  return ( SelPermInfo (HS.fromList pgCols) (HS.fromList computedFields)
                        tn be mLimit allowAgg depHeaders
         , deps
         )
  where
    tn = _tiName tabInfo
    fieldInfoMap = _tiFieldInfoMap tabInfo
    allowAgg = spAllowAggregations sp
    computedFields = spComputedFields sp
    autoInferredErr = "permissions for relationships are automatically inferred"

type SelPermDef = PermDef SelPerm
type CreateSelPerm = CreatePerm SelPerm
type DropSelPerm = DropPerm SelPerm

type instance PermInfo SelPerm = SelPermInfo

dropSelPermP2
  :: (CacheRWM m, MonadTx m)
  => DropSelPerm -> m ()
dropSelPermP2 dp =
  dropPermP2 dp ()

instance IsPerm SelPerm where

  type DropPermP1Res SelPerm = ()

  permAccessor = PASelect

  buildPermInfo ti (PermDef _ a _) =
    buildSelPermInfo ti a

  buildDropPermP1Res =
    void . dropPermP1

  addPermP2Setup _ _ _ = return ()

  dropPermP2Setup _ _ = return ()

-- Update constraint
data UpdPerm
  = UpdPerm
  { ucColumns :: !PermColSpec -- Allowed columns
  , ucSet     :: !(Maybe (ColumnValues Value)) -- Preset columns
  , ucFilter  :: !BoolExp     -- Filter expression
  } deriving (Show, Eq, Lift, Generic)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''UpdPerm)

type UpdPermDef = PermDef UpdPerm
type CreateUpdPerm = CreatePerm UpdPerm


buildUpdPermInfo
  :: (QErrM m, CacheRM m)
  => TableInfo PGColumnInfo
  -> UpdPerm
  -> m (WithDeps UpdPermInfo)
buildUpdPermInfo tabInfo (UpdPerm colSpec set fltr) = do
  (be, beDeps) <- withPathK "filter" $
    procBoolExp tn fieldInfoMap fltr

  (setColsSQL, setHeaders, setColDeps) <- procSetObj tabInfo set

  -- check if the columns exist
  void $ withPathK "columns" $ indexedForM updCols $ \updCol ->
       askPGType fieldInfoMap updCol relInUpdErr

  let updColDeps = map (mkColDep DRUntyped tn) updCols
      deps = mkParentDep tn : beDeps ++ updColDeps ++ setColDeps
      depHeaders = getDependentHeaders fltr
      reqHeaders = depHeaders `union` setHeaders
      updColsWithoutPreSets = updCols \\ HM.keys setColsSQL

  return (UpdPermInfo (HS.fromList updColsWithoutPreSets) tn be setColsSQL reqHeaders, deps)

  where
    tn = _tiName tabInfo
    fieldInfoMap = _tiFieldInfoMap tabInfo
    updCols     = convColSpec fieldInfoMap colSpec
    relInUpdErr = "relationships can't be used in update"

type instance PermInfo UpdPerm = UpdPermInfo

type DropUpdPerm = DropPerm UpdPerm

dropUpdPermP2
  :: (CacheRWM m, MonadTx m)
  => DropUpdPerm -> m ()
dropUpdPermP2 dp = dropPermP2 dp ()

instance IsPerm UpdPerm where

  type DropPermP1Res UpdPerm = ()

  permAccessor = PAUpdate

  buildPermInfo ti (PermDef _ a _) =
    buildUpdPermInfo ti a

  addPermP2Setup _ _ _ = return ()

  buildDropPermP1Res =
    void . dropPermP1

  dropPermP2Setup _ _ = return ()

-- Delete permission
data DelPerm
  = DelPerm { dcFilter :: !BoolExp }
  deriving (Show, Eq, Lift, Generic)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''DelPerm)

type DelPermDef = PermDef DelPerm
type CreateDelPerm = CreatePerm DelPerm

buildDelPermInfo
  :: (QErrM m, CacheRM m)
  => TableInfo PGColumnInfo
  -> DelPerm
  -> m (WithDeps DelPermInfo)
buildDelPermInfo tabInfo (DelPerm fltr) = do
  (be, beDeps) <- withPathK "filter" $
    procBoolExp tn fieldInfoMap  fltr
  let deps = mkParentDep tn : beDeps
      depHeaders = getDependentHeaders fltr
  return (DelPermInfo tn be depHeaders, deps)
  where
    tn = _tiName tabInfo
    fieldInfoMap = _tiFieldInfoMap tabInfo

type DropDelPerm = DropPerm DelPerm

dropDelPermP2 :: (CacheRWM m, MonadTx m) => DropDelPerm -> m ()
dropDelPermP2 dp = dropPermP2 dp ()

type instance PermInfo DelPerm = DelPermInfo

instance IsPerm DelPerm where

  type DropPermP1Res DelPerm = ()

  permAccessor = PADelete

  buildPermInfo ti (PermDef _ a _) =
    buildDelPermInfo ti a

  addPermP2Setup _ _ _ = return ()

  buildDropPermP1Res =
    void . dropPermP1

  dropPermP2Setup _ _ = return ()

data SetPermComment
  = SetPermComment
  { apTable      :: !QualifiedTable
  , apRole       :: !RoleName
  , apPermission :: !PermType
  , apComment    :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase) ''SetPermComment)

setPermCommentP1 :: (UserInfoM m, QErrM m, CacheRM m) => SetPermComment -> m ()
setPermCommentP1 (SetPermComment qt rn pt _) = do
  tabInfo <- askTabInfo qt
  action tabInfo
  where
    action tabInfo = case pt of
      PTInsert -> assertPermDefined rn PAInsert tabInfo
      PTSelect -> assertPermDefined rn PASelect tabInfo
      PTUpdate -> assertPermDefined rn PAUpdate tabInfo
      PTDelete -> assertPermDefined rn PADelete tabInfo

setPermCommentP2 :: (QErrM m, MonadTx m) => SetPermComment -> m EncJSON
setPermCommentP2 apc = do
  liftTx $ setPermCommentTx apc
  return successMsg

runSetPermComment
  :: (QErrM m, CacheRM m, MonadTx m, UserInfoM m)
  => SetPermComment -> m EncJSON
runSetPermComment defn =  do
  setPermCommentP1 defn
  setPermCommentP2 defn

setPermCommentTx
  :: SetPermComment
  -> Q.TxE QErr ()
setPermCommentTx (SetPermComment (QualifiedObject sn tn) rn pt comment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE hdb_catalog.hdb_permission
           SET comment = $1
           WHERE table_schema =  $2
             AND table_name = $3
             AND role_name = $4
             AND perm_type = $5
                |] (comment, sn, tn, rn, permTypeToCode pt) True

purgePerm
  :: (CacheRWM m, MonadTx m)
  => QualifiedTable -> RoleName -> PermType -> m ()
purgePerm qt rn pt =
  case pt of
    PTInsert -> dropInsPermP2 dp $ buildViewName qt rn PTInsert
    PTSelect -> dropSelPermP2 dp
    PTUpdate -> dropUpdPermP2 dp
    PTDelete -> dropDelPermP2 dp
  where
    dp :: DropPerm a
    dp = DropPerm qt rn

rebuildPermInfo
  :: (QErrM m, CacheRWM m, MonadTx m)
  => QualifiedTable -> RoleName -> PermType -> m ()
rebuildPermInfo qt rn pt = do
  (pDef, comment) <- liftTx $ fetchPermDef qt rn pt
  case pt of
    PTInsert -> do
      perm <- decodeValue pDef
      updatePerm PAInsert $ PermDef rn perm comment
    PTSelect -> do
      perm <- decodeValue pDef
      updatePerm PASelect $ PermDef rn perm comment
    PTUpdate -> do
      perm <- decodeValue pDef
      updatePerm PAUpdate $ PermDef rn perm comment
    PTDelete -> do
      perm <- decodeValue pDef
      updatePerm PADelete $ PermDef rn perm comment

  where
    updatePerm :: (QErrM m, CacheRWM m, IsPerm a)
               => PermAccessor (PermInfo a) -> PermDef a -> m ()
    updatePerm pa perm = do
      delPermFromCache pa rn qt
      tabInfo <- askTabInfo qt
      (permInfo, deps) <- addPermP1 tabInfo perm
      addPermToCache qt rn pa permInfo deps

fetchPermDef
  :: QualifiedTable
  -> RoleName
  -> PermType
  -> Q.TxE QErr (Value, Maybe T.Text)
fetchPermDef (QualifiedObject sn tn) rn pt =
 (first Q.getAltJ .  Q.getRow) <$> Q.withQE defaultTxErrorHandler
      [Q.sql|
            SELECT perm_def::json, comment
              FROM hdb_catalog.hdb_permission
             WHERE table_schema = $1
               AND table_name = $2
               AND role_name = $3
               AND perm_type = $4
            |] (sn, tn, rn, permTypeToCode pt) True
