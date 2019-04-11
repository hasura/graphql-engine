module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema
  , runRemoveRemoteSchema
  , runGetRemoteSchemaInfo
  , writeRemoteSchemasToCache
  , refreshGCtxMapInSchema
  , fetchRemoteSchemas
  , addRemoteSchemaP2
  , validateRemoteSchema
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.HashMap.Strict           as Map
import qualified Database.PG.Query             as Q

import           Data.List
import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Context        as GC
import qualified Hasura.GraphQL.Schema         as GS
import qualified Hasura.GraphQL.Validate.Types as VT
import qualified Language.GraphQL.Draft.Syntax as G

runAddRemoteSchema
  :: ( QErrM m, UserInfoM m, CacheRWM m, MonadTx m
     , MonadIO m
     , HasHttpManager m
     )
  => AddRemoteSchemaQuery -> m EncJSON
runAddRemoteSchema q = do
  adminOnly
  addRemoteSchemaP2 q

addRemoteSchemaP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , HasHttpManager m
     )
  => AddRemoteSchemaQuery
  -> m EncJSON
addRemoteSchemaP2 q@(AddRemoteSchemaQuery name def _) = do
  rsi <- validateRemoteSchema name def
  manager <- askHttpManager
  sc <- askSchemaCache
  let defRemoteGCtx = scDefaultRemoteGCtx sc
  remoteGCtx <- fetchRemoteSchema manager rsi
  newDefGCtx <- mergeGCtx defRemoteGCtx $ convRemoteGCtx remoteGCtx
  newHsraGCtxMap <- GS.mkGCtxMap (scTables sc) (scFunctions sc)
  newGCtxMap <- mergeRemoteSchema newHsraGCtxMap newDefGCtx
  liftTx $ addRemoteSchemaToCatalog q
  addRemoteSchemaToCache newGCtxMap newDefGCtx name rsi
  return successMsg

validateRemoteSchema
  :: (MonadError QErr m, MonadIO m, MonadTx m)
  => RemoteSchemaName -> RemoteSchemaDef
  -> m RemoteSchemaInfo
validateRemoteSchema name (RemoteSchemaDef mUrl mUrlEnv hdrC fwdHdrs) = do
  when (name == "") $ throw400 InvalidParams "remote schema name can't be empty"
  -- should we do this validation here?
  -- mSchema <- liftTx $ fetchRemoteSchemaDef name
  -- onJust mSchema $
  --   const $ throw400 AlreadyExists $ name <> " remote schema already exists"

  case (mUrl, mUrlEnv) of
    (Just url, Nothing)    ->
      return $ RemoteSchemaInfo name url hdrs fwdHdrs
    (Nothing, Just urlEnv) -> do
      url <- getUrlFromEnv urlEnv
      return $ RemoteSchemaInfo name url hdrs fwdHdrs
    (Nothing, Nothing)     ->
        throw400 InvalidParams "both `url` and `url_from_env` can't be empty"
    (Just _, Just _)       ->
        throw400 InvalidParams "both `url` and `url_from_env` can't be present"
  where hdrs = fromMaybe [] hdrC


addRemoteSchemaToCache
  :: CacheRWM m
  => GS.GCtxMap
  -> GS.GCtx
  -> RemoteSchemaName
  -> RemoteSchemaInfo
  -> m ()
addRemoteSchemaToCache gCtxMap defGCtx name rmDef = do
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
  writeSchemaCache sc { scRemoteResolvers = Map.insert name rmDef resolvers
                      , scGCtxMap = gCtxMap
                      , scDefaultRemoteGCtx = defGCtx
                      }

writeRemoteSchemasToCache
  :: CacheRWM m
  => GS.GCtxMap -> RemoteSchemaMap -> m ()
writeRemoteSchemasToCache gCtxMap resolvers = do
  sc <- askSchemaCache
  writeSchemaCache sc { scRemoteResolvers = resolvers
                      , scGCtxMap = gCtxMap
                      }

refreshGCtxMapInSchema
  :: (CacheRWM m, MonadIO m, MonadError QErr m, HasHttpManager m)
  => m ()
refreshGCtxMapInSchema = do
  sc <- askSchemaCache
  gCtxMap <- GS.mkGCtxMap (scTables sc) (scFunctions sc)
  httpMgr <- askHttpManager
  (mergedGCtxMap, defGCtx) <-
    mergeSchemas (scRemoteResolvers sc) gCtxMap httpMgr
  writeSchemaCache sc { scGCtxMap = mergedGCtxMap
                      , scDefaultRemoteGCtx = defGCtx }

runRemoveRemoteSchema
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m)
  => RemoveRemoteSchemaQuery -> m EncJSON
runRemoveRemoteSchema q = do
  adminOnly
  removeRemoteSchemaP2 q

removeRemoteSchemaP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , HasHttpManager m
     )
  => RemoveRemoteSchemaQuery
  -> m EncJSON
removeRemoteSchemaP2 (RemoveRemoteSchemaQuery name) = do
  mSchema <- liftTx $ fetchRemoteSchemaDef name
  void $ liftMaybe (err400 NotExists "no such remote schema") mSchema

  hMgr <- askHttpManager
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
      newResolvers = Map.filterWithKey (\n _ -> n /= name) resolvers

  newGCtxMap <- GS.mkGCtxMap (scTables sc) (scFunctions sc)
  (mergedGCtxMap, defGCtx) <- mergeSchemas newResolvers newGCtxMap hMgr
  removeRemoteSchemaFromCache newResolvers mergedGCtxMap defGCtx
  liftTx $ removeRemoteSchemaFromCatalog name
  return successMsg

removeRemoteSchemaFromCache
  :: CacheRWM m => RemoteSchemaMap -> GS.GCtxMap -> GS.GCtx -> m ()
removeRemoteSchemaFromCache newResolvers gCtxMap defGCtx = do
  sc <- askSchemaCache
  writeSchemaCache sc { scRemoteResolvers = newResolvers
                      , scGCtxMap = gCtxMap
                      , scDefaultRemoteGCtx = defGCtx
                      }

addRemoteSchemaToCatalog
  :: AddRemoteSchemaQuery
  -> Q.TxE QErr ()
addRemoteSchemaToCatalog (AddRemoteSchemaQuery name def comment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.remote_schemas
      (name, definition, comment)
      VALUES ($1, $2, $3)
  |] (name, Q.AltJ $ J.toJSON def, comment) True


removeRemoteSchemaFromCatalog :: Text -> Q.TxE QErr ()
removeRemoteSchemaFromCatalog name =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.remote_schemas
      WHERE name = $1
  |] (Identity name) True


fetchRemoteSchemaDef :: Text -> Q.TxE QErr (Maybe RemoteSchemaDef)
fetchRemoteSchemaDef name =
  fmap (fromRow . runIdentity) <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
     SELECT definition from hdb_catalog.remote_schemas
       WHERE name = $1
     |] (Identity name) True
  where
    fromRow (Q.AltJ def) = def

fetchRemoteSchemas :: Q.TxE QErr [AddRemoteSchemaQuery]
fetchRemoteSchemas =
  map fromRow <$> Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT name, definition, comment
       FROM hdb_catalog.remote_schemas
     |] () True
  where
    fromRow (n, Q.AltJ def, comm) = AddRemoteSchemaQuery n def comm


data ObjFldInfoPruned
 = ObjFldInfoPruned
  { _fipDesc       :: !(Maybe G.Description)
  , _fipName       :: !G.Name
  , _fipType       :: !G.GType
  , _fipInputTypes :: !(Map.HashMap G.Name G.GType)
  } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''ObjFldInfoPruned)

data RemoteSchemaFields
  = RemoteSchemaFields
  { _rsfSchemaName :: !RemoteSchemaName
  , _rsfFields     :: ![ObjFldInfoPruned]
  }

$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''RemoteSchemaFields)

runGetRemoteSchemaInfo
  :: (QErrM m, UserInfoM m, CacheRM m)
  => GetRemoteSchemaInfoQuery -> m EncJSON
runGetRemoteSchemaInfo _ = do
  adminOnly
  sc <- askSchemaCache
  let rGCtx = scDefaultRemoteGCtx sc
      queryMap = VT._otiFields $ GC._gQueryRoot rGCtx
      mutMap = fromMaybe Map.empty (VT._otiFields <$> GC._gMutRoot rGCtx )
      subMap = fromMaybe Map.empty (VT._otiFields <$> GC._gSubRoot rGCtx)
      queryFields = Map.elems queryMap
      queryTypLocs = nub $ flip map queryFields VT._fiLoc
      queryFldsByTyp = flip map queryTypLocs $
        (\ty -> (getSName ty, filter (\fld -> VT._fiLoc fld == ty) queryFields)
        )
      queryFldInfos = map pruneFields queryFldsByTyp
      remoteSchemaInfo = flip map queryFldInfos (\(n, flds) -> RemoteSchemaFields n flds )
  return $ encJFromJValue $ J.toJSON remoteSchemaInfo
  where
    getSName typLoc = case typLoc of
      VT.HasuraType     -> "hasura"
      VT.RemoteType rsi -> rsName rsi
    pruneFields (rsName, xs) = (rsName, map pruneField xs)
    pruneField (VT.ObjFldInfo desc name params ty loc) = ObjFldInfoPruned desc name ty (convInputParams params)
    convInputParams inpParams = Map.map (VT._iviType ) inpParams

