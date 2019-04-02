module Hasura.RQL.DDL.Relationship.Remote where

import           Data.Aeson.Types
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Instances.TH.Lift             ()

import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DDL.Relationship

import qualified Data.HashMap.Strict           as Map
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
  remoteFieldInfo <- getRemoteField gctx (rruRemoteField ru)
  inputFieldInfo <- getInputField remoteFieldInfo (rruInputField ru)
  assertInputPath inputFieldInfo (rruInputPath ru)
  where
    getRemoteField gctx' fieldName = do
      let queryRoot = GS._gQueryRoot gctx'
          fieldMap = _otiFields queryRoot
          fieldM = Map.lookup fieldName fieldMap
      field <- maybe (throw400 ValidationFailed ("field: " <> G.unName fieldName <> " not found")) return fieldM
      let typeLoc = _fiLoc field
      case typeLoc of
        HasuraType -> throw400 ValidationFailed ("field: " <> G.unName fieldName <> " not found in any remote schema")
        RemoteType _ _ -> return field

    getInputField remoteFieldInfo inputFieldName = do
      let params = _fiParams remoteFieldInfo
          inputParam = Map.lookup inputFieldName params
      maybe (throw400 ValidationFailed ("input field: " <> G.unName inputFieldName <> "not found")) return inputParam

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
  liftTx $ persistRel qt rn RemoteRel (toJSON u) comment

remoteRelP2Setup
  :: (QErrM m, CacheRWM m, MonadTx m)
  => QualifiedTable -> RemoteRelDef -> m ()
remoteRelP2Setup qt (RelDef rn ru _) = do
  let tableDep = SchemaDependency (SOTable qt) "parent table"
      col = rruColumn ru
      colDep =  SchemaDependency (SOTableObj qt $ TOCol col) "join col"
      deps = [tableDep, colDep]
  remScName <- getRemoteSchemaNameFromField (rruRemoteField ru)
  let relInfo = RemoteRelInfo rn "adfa" remScName
  addRemoteRelToCache rn relInfo deps qt

getRemoteSchemaNameFromField
  :: (QErrM m, CacheRM m)
  => G.Name -> m RemoteSchemaName
getRemoteSchemaNameFromField fieldName = do
  sc <- askSchemaCache
  let gctx = scDefaultRemoteGCtx sc
      queryRoot = GS._gQueryRoot gctx
      fieldMap = _otiFields queryRoot
      fieldM = Map.lookup fieldName fieldMap
  field <- maybe (throw400 ValidationFailed ("field: " <> G.unName fieldName <> "not found")) return fieldM
  case _fiLoc field of
    HasuraType -> throw400 ValidationFailed ("field: " <> G.unName fieldName <> "not found in any remote schema")
    RemoteType name _ -> return name
