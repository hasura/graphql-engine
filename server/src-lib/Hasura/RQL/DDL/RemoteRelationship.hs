{-# LANGUAGE RecordWildCards #-}

module Hasura.RQL.DDL.RemoteRelationship
  ( runCreateRemoteRelationship
  , runCreateRemoteRelationshipP1
  )
where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.RemoteRelationship.Validate
import           Hasura.RQL.Types

import qualified Data.HashMap.Strict                        as Map
import qualified Data.Text                                  as T
import           Instances.TH.Lift                          ()

runCreateRemoteRelationship ::
     (MonadTx m, CacheRWM m, UserInfoM m) => RemoteRelationship -> m EncJSON
runCreateRemoteRelationship remoteRelationship = do
  adminOnly
  _remoteField <- runCreateRemoteRelationshipP1 remoteRelationship
  pure successMsg

runCreateRemoteRelationshipP1 ::
     (MonadTx m, CacheRM m) => RemoteRelationship -> m RemoteField
runCreateRemoteRelationshipP1 remoteRel@RemoteRelationship{..}= do
  sc <- askSchemaCache
  case Map.lookup
         rrRemoteSchema
         (scRemoteSchemas sc) of
    Just rsCtx -> do
      tableInfo <- onNothing (Map.lookup rrTable $ scTables sc) $ throw400 NotFound "table not found"
      validation <-
        getCreateRemoteRelationshipValidation remoteRel rsCtx tableInfo
      case validation of
        Left err          -> throw400 RemoteSchemaError (T.pack (show err))
        Right remoteField -> pure remoteField
    Nothing -> throw400 RemoteSchemaError "No such remote schema"
