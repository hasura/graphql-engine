{-# LANGUAGE ViewPatterns #-}

module Hasura.RQL.DDL.RemoteRelationship
  ( runCreateRemoteRelationship
  , runCreateRemoteRelationshipP1
  )
where

import           Hasura.GraphQL.Validate.Types

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.RemoteRelationship.Validate
import           Hasura.RQL.Types

import qualified Data.HashMap.Strict                        as HM
import qualified Data.Text                                  as T
import           Instances.TH.Lift                          ()

runCreateRemoteRelationship ::
     (MonadTx m, CacheRWM m, UserInfoM m) => RemoteRelationship -> m EncJSON
runCreateRemoteRelationship remoteRelationship = do
  adminOnly
  (_remoteField, _additionalTypesMap) <-
    runCreateRemoteRelationshipP1 remoteRelationship
  pure successMsg

runCreateRemoteRelationshipP1 ::
     (MonadTx m, CacheRM m) => RemoteRelationship -> m (RemoteField, TypeMap)
runCreateRemoteRelationshipP1 remoteRelationship = do
  sc <- askSchemaCache
  case HM.lookup
         (rtrRemoteSchema remoteRelationship)
         (scRemoteSchemas sc) of
    Just {} -> do
      validation <-
        getCreateRemoteRelationshipValidation remoteRelationship
      case validation of
        Left err -> throw400 RemoteSchemaError (T.pack (show err))
        Right (remoteField, additionalTypesMap) ->
          pure (remoteField, additionalTypesMap)
    Nothing -> throw400 RemoteSchemaError "No such remote schema"
