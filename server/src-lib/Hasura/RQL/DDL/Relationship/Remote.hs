module Hasura.RQL.DDL.Relationship.Remote where

import           Data.Aeson.Types
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Instances.TH.Lift             ()

import           Hasura.GraphQL.Validate.Types as VT
import           Hasura.RQL.DDL.Relationship

import qualified Data.HashMap.Strict           as Map
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Hasura.GraphQL.Context        as GC
import qualified Hasura.GraphQL.Schema         as GS
import qualified Language.GraphQL.Draft.Syntax as G

runCreateRemoteRel
  :: (QErrM m, CacheRWM m, MonadTx m , UserInfoM m)
  => CreateRemoteRel -> m EncJSON
runCreateRemoteRel defn = do
  createRemoteRelP1 defn
  createRemoteRelP2 defn

createRemoteRelP1 :: (UserInfoM m, QErrM m, CacheRM m) => CreateRemoteRel -> m ()
createRemoteRelP1 (WithTable qt rd) = do
  adminOnly
  validateRemoteRel qt rd

getRemoteField :: (MonadError QErr m) => GC.GCtx -> G.Name -> Maybe G.Name -> m ObjFldInfo
getRemoteField gctx fieldName namespaceM = do
  let queryRoot = GS._gQueryRoot gctx
      fieldMap = _otiFields queryRoot
  fieldM <- case namespaceM of
           Nothing -> return $ Map.lookup fieldName fieldMap
           Just ns -> do
             let nsFldM = Map.lookup ns fieldMap
             nsFld <- onNothing nsFldM $ throw400 ValidationFailed ("namespace: " <> G.unName ns <> " not found")
             let fldM = getSelFieldFromNS gctx (VT._fiTy nsFld) fieldName
             return fldM
  field <- maybe (throw400 ValidationFailed ("field: " <> G.unName fieldName <> " not found")) return fieldM
  let typeLoc = _fiLoc field
  case typeLoc of
    HasuraType -> throw400 ValidationFailed ("field: " <> G.unName fieldName <> " not found in any remote schema")
    RemoteType _ -> return field
  where
    getSelFieldFromNS gCtx gTy fldName =
      let baseTy = getBaseTy gTy
          tyInfo = Map.lookup baseTy (GC._gTypes gCtx)
      in case tyInfo of
        Just (VT.TIObj tyObjInfo) -> Map.lookup fldName (VT._otiFields tyObjInfo)
        _ -> Nothing

validateRemoteRel
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> RemoteRelDef -> m ()
validateRemoteRel qt (RelDef rn ru _) = do
  sc <- askSchemaCache
  tabInfo <- askTabInfo qt
  checkForColConfilct tabInfo (fromRel rn)
  let fim = tiFieldInfoMap tabInfo
  assertPGCol fim "" (rruColumn ru)
  let gctx = scDefaultRemoteGCtx sc
  remoteFieldInfo <- getRemoteField gctx (rruRemoteField ru) (rruNamespace ru)
  inputFieldInfo <- getInputField remoteFieldInfo (rruInputField ru)
  assertInputPath inputFieldInfo (rruInputPath ru)
  where
    getInputField remoteFieldInfo inputFieldName = do
      let params = _fiParams remoteFieldInfo
          inputParam = Map.lookup inputFieldName params
      maybe (throw400 ValidationFailed ("input field: " <> G.unName inputFieldName <> " not found")) return inputParam

    assertInputPath inputFieldInfo inputPath = return ()

createRemoteRelP2
  :: (QErrM m, CacheRWM m, MonadTx m) => CreateRemoteRel -> m EncJSON
createRemoteRelP2 (WithTable qt rd) = do
  remoteRelP2 qt rd
  return successMsg

remoteRelP2
  :: (QErrM m, CacheRWM m, MonadTx m)
  => QualifiedTable -> RemoteRelDef -> m ()
remoteRelP2 qt rd@(RelDef rn u comment) = do
  remoteRelP2Setup qt rd
  liftTx $ persistRemoteRel qt rn (toJSON u) comment
  where
    persistRemoteRel :: QualifiedTable
           -> RelName
           -> Value
           -> Maybe T.Text
           -> Q.TxE QErr ()
    persistRemoteRel (QualifiedObject sn tn) rn' relDef comment' =
      Q.unitQE defaultTxErrorHandler [Q.sql|
               INSERT INTO
                 hdb_catalog.hdb_remote_relationship
                 (table_schema, table_name, rel_name, rel_type, rel_def, comment)
               VALUES ($1, $2, $3, 'remote', $4 , $5)
                 |] (sn, tn, rn', Q.AltJ relDef, comment') True

remoteRelP2Setup
  :: (QErrM m, CacheRWM m, MonadTx m)
  => QualifiedTable -> RemoteRelDef -> m ()
remoteRelP2Setup qt (RelDef rn ru _) = do
  let tableDep = SchemaDependency (SOTable qt) "parent table"
      col = rruColumn ru
      colDep =  SchemaDependency (SOTableObj qt $ TOCol col) "join col"
      deps = [tableDep, colDep]
  sc <- askSchemaCache
  let gctx = scDefaultRemoteGCtx sc
      namespace = rruNamespace ru
      remoteFldName = rruRemoteField ru
  remScName <- rsName <$> getRemoteSchemaInfoFromField gctx (rruRemoteField ru) (rruNamespace ru)
  remoteObjFieldInfo <- getRemoteField gctx remoteFldName namespace
  let remoteInpParams = Map.map reduceInpVal (_fiParams remoteObjFieldInfo)
      remoteFldInfo = RemoteFldInfo namespace remoteFldName (_fiTy remoteObjFieldInfo) remoteInpParams
  let relInfo = RemoteRelInfo rn remScName qt (rruColumn ru) remoteFldInfo (rruInputField ru) (rruInputPath ru)
  addRemoteRelToCache rn relInfo deps qt
  where
    -- getRemoteField gctx' fieldName = do
    --   let queryRoot = GS._gQueryRoot gctx'
    --       fieldMap = _otiFields queryRoot
    --       fieldM = Map.lookup fieldName fieldMap
    --   field <- maybe (throw400 ValidationFailed ("field: " <> G.unName fieldName <> " not found")) return fieldM
    --   let typeLoc = _fiLoc field
    --   case typeLoc of
    --     HasuraType -> throw400 ValidationFailed ("field: " <> G.unName fieldName <> " not found in any remote schema")
    --     RemoteType _ -> return field

    reduceInpVal ivi = InpValInfo'
                       { __iviDesc = _iviDesc ivi
                       , __iviName = _iviName ivi
                       , __iviType = _iviType ivi
                       }

getRemoteSchemaInfoFromField
  :: (QErrM m)
  => GS.GCtx -> G.Name -> Maybe G.Name -> m RemoteSchemaInfo
getRemoteSchemaInfoFromField gCtx fieldName nsM = do
  let queryRoot = GS._gQueryRoot gCtx
      fieldMap = _otiFields queryRoot
      fieldM = maybe (Map.lookup fieldName fieldMap) (flip Map.lookup fieldMap) nsM
  field <- maybe (throw400 ValidationFailed ("field: " <> G.unName fieldName <> " not found")) return fieldM
  case _fiLoc field of
    HasuraType -> throw400 ValidationFailed ("field: " <> G.unName fieldName <> " not found in any remote schema")
    RemoteType rsi -> return rsi

runDropRemoteRel
  :: (QErrM m, CacheRWM m, MonadTx m, UserInfoM m)
  => DropRemoteRel -> m EncJSON
runDropRemoteRel (DropRemoteRel qt name _) = do
  adminOnly
  delRelFromCache name qt
  liftTx $ delRemoteRelFromCatalog qt name
  return successMsg

delRemoteRelFromCatalog
  :: QualifiedTable
  -> RelName
  -> Q.TxE QErr ()
delRemoteRelFromCatalog (QualifiedObject sn tn) rn =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           DELETE FROM
                  hdb_catalog.hdb_remote_relationship
           WHERE table_schema =  $1
             AND table_name = $2
             AND rel_name = $3
                |] (sn, tn, rn) True
