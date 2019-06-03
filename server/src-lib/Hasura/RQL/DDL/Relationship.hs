module Hasura.RQL.DDL.Relationship
  ( validateObjRel
  , objRelP2Setup
  , objRelP2
  , validateArrRel
  , arrRelP2Setup
  , arrRelP2
  , delRelFromCatalog
  , validateRelP1
  , runCreateObjRel
  , runCreateArrRel
  , runDropRel
  , runSetRelComment
  , module Hasura.RQL.DDL.Relationship.Types
  )
where

import qualified Database.PG.Query                 as Q

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Permission         (purgePerm)
import           Hasura.RQL.DDL.Relationship.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Data.Aeson.Types
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import qualified Data.Map.Strict                   as M
import qualified Data.Text                         as T
import           Data.Tuple                        (swap)
import           Instances.TH.Lift                 ()

validateManualConfig
  :: (QErrM m, CacheRM m)
  => FieldInfoMap
  -> RelManualConfig
  -> m ()
validateManualConfig fim rm = do
  let colMapping = M.toList $ rmColumns rm
      remoteQt = rmTable rm
  remoteTabInfo <- askTabInfo remoteQt
  let remoteFim = tiFieldInfoMap remoteTabInfo
  forM_ colMapping $ \(lCol, rCol) -> do
    assertPGCol fim "" lCol
    assertPGCol remoteFim "" rCol
    -- lColType <- askPGType fim lCol ""
    -- rColType <- askPGType remoteFim rCol ""
    -- when (lColType /= rColType) $
    --   throw400 $ mconcat
    --   [ "the types of columns " <> lCol <<> ", " <>> rCol
    --   , " do not match"
    --   ]

persistRel :: QualifiedTable
           -> RelName
           -> RelType
           -> Value
           -> Maybe T.Text
           -> Q.TxE QErr ()
persistRel (QualifiedObject sn tn) rn relType relDef comment =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           INSERT INTO
                  hdb_catalog.hdb_relationship
                  (table_schema, table_name, rel_name, rel_type, rel_def, comment)
           VALUES ($1, $2, $3, $4, $5 :: jsonb, $6)
                |] (sn, tn, rn, relTypeToTxt relType, Q.AltJ relDef, comment) True

checkForFldConfilct
  :: (MonadError QErr m)
  => TableInfo
  -> FieldName
  -> m ()
checkForFldConfilct tabInfo f =
  case HM.lookup f (tiFieldInfoMap tabInfo) of
    Just _ -> throw400 AlreadyExists $ mconcat
      [ "column/relationship " <>> f
      , " of table " <>> tiName tabInfo
      , " already exists"
      ]
    Nothing -> return ()

validateObjRel
  :: (QErrM m, CacheRM m)
  => QualifiedTable
  -> ObjRelDef
  -> m ()
validateObjRel qt (RelDef rn ru _) = do
  tabInfo <- askTabInfo qt
  checkForFldConfilct tabInfo (fromRel rn)
  let fim = tiFieldInfoMap tabInfo
  case ru of
    RUFKeyOn cn                      -> assertPGCol fim "" cn
    RUManual (ObjRelManualConfig rm) -> validateManualConfig fim rm

createObjRelP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => CreateObjRel
  -> m ()
createObjRelP1 (WithTable qt rd) = do
  adminOnly
  validateObjRel qt rd

objRelP2Setup
  :: (QErrM m, CacheRWM m)
  => QualifiedTable -> HS.HashSet ForeignKey -> RelDef ObjRelUsing -> m ()
objRelP2Setup qt fkeys (RelDef rn ru _) = do
  (relInfo, deps) <- case ru of
    RUManual (ObjRelManualConfig rm) -> do
      let refqt = rmTable rm
          (lCols, rCols) = unzip $ M.toList $ rmColumns rm
          deps  = map (\c -> SchemaDependency (SOTableObj qt $ TOCol c) "lcol") lCols
                  <> map (\c -> SchemaDependency (SOTableObj refqt $ TOCol c) "rcol") rCols
      return (RelInfo rn ObjRel (zip lCols rCols) refqt True, deps)
    RUFKeyOn cn -> do
      -- TODO: validation should account for this too
      ForeignKey _ refqt _ consName colMap <-
        getRequiredFkey cn fkeys $ \fk -> _fkTable fk == qt

      let deps = [ SchemaDependency (SOTableObj qt $ TOCons consName) "fkey"
                 , SchemaDependency (SOTableObj qt $ TOCol cn) "using_col"
                 -- this needs to be added explicitly to handle the remote table
                 -- being untracked. In this case, neither the using_col nor
                 -- the constraint name will help.
                 , SchemaDependency (SOTable refqt) "remote_table"
                 ]
          colMapping = HM.toList colMap
      void $ askTabInfo refqt
      return (RelInfo rn ObjRel colMapping refqt False, deps)
  addRelToCache rn relInfo deps qt

objRelP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     )
  => QualifiedTable
  -> ObjRelDef
  -> m ()
objRelP2 qt rd@(RelDef rn ru comment) = do
  fkeys <- liftTx $ fetchTableFkeys qt
  objRelP2Setup qt fkeys rd
  liftTx $ persistRel qt rn ObjRel (toJSON ru) comment

createObjRelP2
  :: (QErrM m, CacheRWM m, MonadTx m) => CreateObjRel -> m EncJSON
createObjRelP2 (WithTable qt rd) = do
  objRelP2 qt rd
  return successMsg

runCreateObjRel
  :: (QErrM m, CacheRWM m, MonadTx m , UserInfoM m)
  => CreateObjRel -> m EncJSON
runCreateObjRel defn = do
  createObjRelP1 defn
  createObjRelP2 defn

createArrRelP1 :: (UserInfoM m, QErrM m, CacheRM m) => CreateArrRel -> m ()
createArrRelP1 (WithTable qt rd) = do
  adminOnly
  validateArrRel qt rd

validateArrRel
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> ArrRelDef -> m ()
validateArrRel qt (RelDef rn ru _) = do
  tabInfo <- askTabInfo qt
  checkForFldConfilct tabInfo (fromRel rn)
  let fim = tiFieldInfoMap tabInfo
  case ru of
    RUFKeyOn (ArrRelUsingFKeyOn remoteQt rcn) -> do
      remoteTabInfo <- askTabInfo remoteQt
      let rfim = tiFieldInfoMap remoteTabInfo
      -- Check if 'using' column exists
      assertPGCol rfim "" rcn
    RUManual (ArrRelManualConfig rm) ->
      validateManualConfig fim rm

arrRelP2Setup
  :: (QErrM m, CacheRWM m)
  => QualifiedTable -> HS.HashSet ForeignKey -> ArrRelDef -> m ()
arrRelP2Setup qt fkeys (RelDef rn ru _) = do
  (relInfo, deps) <- case ru of
    RUManual (ArrRelManualConfig rm) -> do
      let refqt = rmTable rm
          (lCols, rCols) = unzip $ M.toList $ rmColumns rm
          deps  = map (\c -> SchemaDependency (SOTableObj qt $ TOCol c) "lcol") lCols
                  <> map (\c -> SchemaDependency (SOTableObj refqt $ TOCol c) "rcol") rCols
      return (RelInfo rn ArrRel (zip lCols rCols) refqt True, deps)
    RUFKeyOn (ArrRelUsingFKeyOn refqt refCol) -> do
      -- TODO: validation should account for this too
      ForeignKey _ _ _ consName colMap <- getRequiredFkey refCol fkeys $
        \fk -> _fkTable fk == refqt && _fkRefTable fk == qt
      let deps = [ SchemaDependency (SOTableObj refqt $ TOCons consName) "remote_fkey"
                 , SchemaDependency (SOTableObj refqt $ TOCol refCol) "using_col"
                 -- we don't need to necessarily track the remote table like we did in
                 -- case of obj relationships as the remote table is indirectly
                 -- tracked by tracking the constraint name and 'using_col'
                 , SchemaDependency (SOTable refqt) "remote_table"
                 ]
          mapping = HM.toList colMap
      return (RelInfo rn ArrRel (map swap mapping) refqt False, deps)
  addRelToCache rn relInfo deps qt

arrRelP2
  :: (QErrM m, CacheRWM m, MonadTx m)
  => QualifiedTable -> ArrRelDef -> m ()
arrRelP2 qt rd@(RelDef rn u comment) = do
  fkeys <- liftTx $ fetchFkeysAsRemoteTable qt
  arrRelP2Setup qt fkeys rd
  liftTx $ persistRel qt rn ArrRel (toJSON u) comment

createArrRelP2
  :: (QErrM m, CacheRWM m, MonadTx m) => CreateArrRel -> m EncJSON
createArrRelP2 (WithTable qt rd) = do
  arrRelP2 qt rd
  return successMsg

runCreateArrRel
  :: (QErrM m, CacheRWM m, MonadTx m , UserInfoM m)
  => CreateArrRel -> m EncJSON
runCreateArrRel defn = do
  createArrRelP1 defn
  createArrRelP2 defn

dropRelP1 :: (UserInfoM m, QErrM m, CacheRM m) => DropRel -> m [SchemaObjId]
dropRelP1 (DropRel qt rn cascade) = do
  adminOnly
  tabInfo <- askTabInfo qt
  _       <- askRelType (tiFieldInfoMap tabInfo) rn ""
  sc      <- askSchemaCache
  let depObjs = getDependentObjs sc relObjId
  when (depObjs /= [] && not (or cascade)) $ reportDeps depObjs
  return depObjs
  where
    relObjId = SOTableObj qt $ TORel rn

purgeRelDep
  :: (CacheRWM m, MonadTx m) => SchemaObjId -> m ()
purgeRelDep (SOTableObj tn (TOPerm rn pt)) =
  purgePerm tn rn pt
purgeRelDep d = throw500 $ "unexpected dependency of relationship : "
                <> reportSchemaObj d

dropRelP2
  :: (QErrM m, CacheRWM m, MonadTx m)
  => DropRel -> [SchemaObjId] -> m EncJSON
dropRelP2 (DropRel qt rn _) depObjs = do
  mapM_ purgeRelDep depObjs
  delRelFromCache rn qt
  liftTx $ delRelFromCatalog qt rn
  return successMsg

runDropRel
  :: (QErrM m, CacheRWM m, MonadTx m , UserInfoM m)
  => DropRel -> m EncJSON
runDropRel defn = do
  depObjs <- dropRelP1 defn
  dropRelP2 defn depObjs

delRelFromCatalog
  :: QualifiedTable
  -> RelName
  -> Q.TxE QErr ()
delRelFromCatalog (QualifiedObject sn tn) rn =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           DELETE FROM
                  hdb_catalog.hdb_relationship
           WHERE table_schema =  $1
             AND table_name = $2
             AND rel_name = $3
                |] (sn, tn, rn) True

validateRelP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => QualifiedTable -> RelName -> m RelInfo
validateRelP1 qt rn = do
  adminOnly
  tabInfo <- askTabInfo qt
  askRelType (tiFieldInfoMap tabInfo) rn ""

setRelCommentP2
  :: (QErrM m, MonadTx m)
  => SetRelComment -> m EncJSON
setRelCommentP2 arc = do
  liftTx $ setRelComment arc
  return successMsg

runSetRelComment
  :: (QErrM m, CacheRWM m, MonadTx m , UserInfoM m)
  => SetRelComment -> m EncJSON
runSetRelComment defn = do
  void $ validateRelP1 qt rn
  setRelCommentP2 defn
  where
    SetRelComment qt rn _ = defn

setRelComment :: SetRelComment
              -> Q.TxE QErr ()
setRelComment (SetRelComment (QualifiedObject sn tn) rn comment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE hdb_catalog.hdb_relationship
           SET comment = $1
           WHERE table_schema =  $2
             AND table_name = $3
             AND rel_name = $4
                |] (comment, sn, tn, rn) True

getRequiredFkey
  :: (QErrM m)
  => PGCol
  -> HS.HashSet ForeignKey
  -> (ForeignKey -> Bool)
  -> m ForeignKey
getRequiredFkey col fkeySet preCondition =
  case filterFkeys of
    []  -> throw400 ConstraintError
          "no foreign constraint exists on the given column"
    [k] -> return k
    _   -> throw400 ConstraintError
           "more than one foreign key constraint exists on the given column"
  where
    filterFkeys = HS.toList $ HS.filter filterFn fkeySet
    filterFn k = preCondition k && HM.keys (_fkColumnMapping k) == [col]

fetchTableFkeys :: QualifiedTable -> Q.TxE QErr (HS.HashSet ForeignKey)
fetchTableFkeys qt@(QualifiedObject sn tn) = do
  r <- Q.listQE defaultTxErrorHandler [Q.sql|
          SELECT f.constraint_name,
                 f.ref_table_table_schema,
                 f.ref_table,
                 f.constraint_oid,
                 f.column_mapping
            FROM hdb_catalog.hdb_foreign_key_constraint f
           WHERE f.table_schema = $1 AND f.table_name = $2
          |] (sn, tn) True
  fmap HS.fromList $
    forM r $ \(constr, refsn, reftn, oid, Q.AltJ colMapping) ->
    return $ ForeignKey qt (QualifiedObject refsn reftn) oid constr colMapping

fetchFkeysAsRemoteTable :: QualifiedTable -> Q.TxE QErr (HS.HashSet ForeignKey)
fetchFkeysAsRemoteTable rqt@(QualifiedObject rsn rtn) = do
  r <- Q.listQE defaultTxErrorHandler [Q.sql|
          SELECT f.table_schema,
                 f.table_name,
                 f.constraint_name,
                 f.constraint_oid,
                 f.column_mapping
            FROM hdb_catalog.hdb_foreign_key_constraint f
           WHERE f.ref_table_table_schema = $1 AND f.ref_table = $2
          |] (rsn, rtn) True
  fmap HS.fromList $
    forM r $ \(sn, tn, constr, oid, Q.AltJ colMapping) ->
    return $ ForeignKey (QualifiedObject sn tn) rqt oid constr colMapping
