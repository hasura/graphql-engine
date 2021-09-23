module Hasura.RQL.DDL.GraphqlSchemaIntrospection where

import           Hasura.Prelude

import           Control.Lens      ((.~))

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.RQL.Types


runSetGraphqlSchemaIntrospectionOptions
  :: (MonadError QErr m, MetadataM m, CacheRWM m)
  => SetGraphqlIntrospectionOptions -> m EncJSON
runSetGraphqlSchemaIntrospectionOptions introspectionOptions = do
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaSetGraphqlIntrospectionOptions .~ introspectionOptions
  return successMsg
