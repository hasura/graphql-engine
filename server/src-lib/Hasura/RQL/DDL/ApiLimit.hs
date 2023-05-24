module Hasura.RQL.DDL.ApiLimit
  ( runRemoveApiLimits,
    runSetApiLimits,
    warningMessage,
    compareTimeLimitWith,
  )
where

import Control.Lens ((.~))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.DDL.Warnings
import Hasura.RQL.Types.ApiLimit
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.Server.Types (MonadGetPolicies (..))

runSetApiLimits ::
  (MonadError QErr m, MetadataM m, CacheRWM m, MonadGetPolicies m) =>
  ApiLimit ->
  m EncJSON
runSetApiLimits al = do
  let userTimeLimitAPILimit = _lGlobal <$> _alTimeLimit al
  -- If both user time limit and cloud limit are present then check if the user time limit API limit is greater than the
  -- cloud time limit API limit. Otheriwse, apply the API limit configuration (without the warning).
  warningResultEither <- compareTimeLimitWith userTimeLimitAPILimit
  case warningResultEither of
    Left warning -> do
      successMsgWithWarning <- successMsgWithWarnings $ warn warning
      setApiLimit successMsgWithWarning
    Right _ -> setApiLimit successMsg
  where
    setApiLimit successMessage = do
      withNewInconsistentObjsCheck
        $ buildSchemaCache
        $ MetadataModifier
        $ metaApiLimits
        .~ al
      return successMessage

-- This function compares the user time_limit and the cloud time_limit (used in both set_api_limit and replace_metadata
-- APIs). The function returns either a metadata warning or `()`
compareTimeLimitWith :: (MonadGetPolicies m) => Maybe MaxTime -> m (Either MetadataWarning ())
compareTimeLimitWith userTimeLimitMaybe = do
  cloudApiTimeLimit <- runGetApiTimeLimit
  let compareTimeLimitResultEither =
        case (userTimeLimitMaybe, cloudApiTimeLimit) of
          (Just userTimeLimitAPILimit, Just cloudTimeLimit) -> do
            if userTimeLimitAPILimit > cloudTimeLimit
              then Left $ warningMessage userTimeLimitAPILimit cloudTimeLimit
              else Right ()
          _ -> Right ()
  pure compareTimeLimitResultEither

-- warning message if the user time limit API limit is greater than the cloud time limit API limit
warningMessage :: MaxTime -> MaxTime -> MetadataWarning
warningMessage userTimeLimit cloudTimeLimit =
  MetadataWarning WCTimeLimitExceededSystemLimit (MOSource defaultSource)
    $ "the configured time limit: "
    <> tshow (seconds $ unMaxTime userTimeLimit)
    <> " exceeds the project time limit: "
    <> tshow (seconds $ unMaxTime cloudTimeLimit)
    <> ". Time limit of "
    <> tshow (seconds $ unMaxTime cloudTimeLimit)
    <> " will be applied"

runRemoveApiLimits ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  m EncJSON
runRemoveApiLimits = do
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaApiLimits
    .~ emptyApiLimit
  return successMsg
