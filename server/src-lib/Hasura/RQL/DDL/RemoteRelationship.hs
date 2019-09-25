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
      let remoteFieldE  = validateRelationship remoteRel (rscGCtx rsCtx) tableInfo
      case remoteFieldE of
        Left err          -> fromValidationError err
        Right remoteField -> pure remoteField
    Nothing -> throw400 NotFound ("no such remote schema: " <> remoteSchemaNameToTxt rrRemoteSchema)
