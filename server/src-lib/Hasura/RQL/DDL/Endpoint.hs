module Hasura.RQL.DDL.Endpoint
  ( runCreateEndpoint,
    runDropEndpoint,
    dropEndpointInMetadata,
  )
where

import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache.Build

runCreateEndpoint ::
  ( MonadError QErr m,
    CacheRWM m,
    MetadataM m
  ) =>
  CreateEndpoint ->
  m EncJSON
runCreateEndpoint endpoint@EndpointMetadata {..} = do
  endpointsMap <- _metaRestEndpoints <$> getMetadata

  InsOrdHashMap.lookup _ceName endpointsMap `for_` \_ ->
    throw400 AlreadyExists
      $ "Endpoint with name: "
      <> toTxt _ceName
      <> " already exists"

  withNewInconsistentObjsCheck
    $ buildSchemaCacheFor (MOEndpoint _ceName)
    $ MetadataModifier
    $ metaRestEndpoints
    %~ InsOrdHashMap.insert _ceName endpoint
  return successMsg

runDropEndpoint ::
  ( MonadError QErr m,
    CacheRWM m,
    MetadataM m
  ) =>
  DropEndpoint ->
  m EncJSON
runDropEndpoint DropEndpoint {..} = do
  checkExists _deName
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ dropEndpointInMetadata _deName
  return successMsg

dropEndpointInMetadata :: EndpointName -> MetadataModifier
dropEndpointInMetadata name =
  MetadataModifier $ metaRestEndpoints %~ InsOrdHashMap.delete name

checkExists :: (MetadataM m, MonadError QErr m) => EndpointName -> m ()
checkExists name = do
  endpointsMap <- _metaRestEndpoints <$> getMetadata
  void
    $ onNothing (InsOrdHashMap.lookup name endpointsMap)
    $ throw400 NotExists
    $ "endpoint with name: "
    <> toTxt name
    <> " does not exist"
