module Hasura.RQL.DDL.GraphqlSchemaIntrospection
  ( runSetGraphqlSchemaIntrospectionOptions,
  )
where

import Control.Lens ((.~))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Instances ()
import Hasura.RQL.Types.SchemaCache.Build

runSetGraphqlSchemaIntrospectionOptions ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  SetGraphqlIntrospectionOptions ->
  m EncJSON
runSetGraphqlSchemaIntrospectionOptions introspectionOptions = do
  let metadataModifier = MetadataModifier $ metaSetGraphqlIntrospectionOptions .~ introspectionOptions
  withNewInconsistentObjsCheck
    $ buildSchemaCache metadataModifier
  return successMsg
