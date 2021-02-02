module Hasura.RQL.DDL.Endpoint
  ( runCreateEndpoint
  , runDropEndpoint
  , dropEndpointInMetadata
  ) where

import           Data.Text.Extended
import           Hasura.EncJSON
import           Hasura.Metadata.Class
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Data.HashMap.Strict.InsOrd as OMap

runCreateEndpoint
  :: ( CacheRWM m
     , MetadataM m
     , MonadMetadataStorageQueryAPI m
     )
  => CreateEndpoint
  -> m EncJSON
runCreateEndpoint endpoint@EndpointMetadata{..} = do

  endpointsMap <- _metaRestEndpoints <$> getMetadata

  OMap.lookup _ceName endpointsMap `for_` \_ ->
     throw400 AlreadyExists $
       "Endpoint with name: " <> toTxt _ceName <> " already exists"

  withNewInconsistentObjsCheck
    $ buildSchemaCacheFor (MOEndpoint _ceName)
    $ MetadataModifier
    $ metaRestEndpoints %~ OMap.insert _ceName endpoint
  return successMsg

runDropEndpoint
  :: ( CacheRWM m
     , MetadataM m
     , MonadMetadataStorageQueryAPI m
     )
  => DropEndpoint
  -> m EncJSON
runDropEndpoint DropEndpoint{..} = do
  checkExists _deName
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ dropEndpointInMetadata _deName
  return successMsg

dropEndpointInMetadata :: EndpointName -> MetadataModifier
dropEndpointInMetadata name =
  MetadataModifier $ metaRestEndpoints %~ OMap.delete name

checkExists :: (MetadataM m, MonadError QErr m) => EndpointName -> m ()
checkExists name = do
  endpointsMap <- _metaRestEndpoints <$> getMetadata
  void $ onNothing (OMap.lookup name endpointsMap) $
    throw400 NotExists $
      "endpoint with name: " <> toTxt name <> " does not exist"
