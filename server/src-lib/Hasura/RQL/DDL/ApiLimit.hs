module Hasura.RQL.DDL.ApiLimit where

import Control.Lens ((.~))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types

runSetApiLimits ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  ApiLimit ->
  m EncJSON
runSetApiLimits al = do
  withNewInconsistentObjsCheck $
    buildSchemaCache $
      MetadataModifier $
        metaApiLimits .~ al
  return successMsg

runRemoveApiLimits ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  m EncJSON
runRemoveApiLimits = do
  withNewInconsistentObjsCheck $
    buildSchemaCache $
      MetadataModifier $
        metaApiLimits .~ emptyApiLimit
  return successMsg
