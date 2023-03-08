-- | Warnings for metadata APIs
--
-- This module provides a mechanism for metadata APIs to emit warnings. An example use of @MonadWarnings@ to emit
-- warnings with success message is given below:
--
-- > import Hasura.RQL.DDL.Warnings
-- >
-- > someMetadataAPIHandler :: args -> m EncJSON
-- > someMetadataAPIHandler args = successMsgWithWarnings $ do
-- >   -- do some stuff
-- >   let warning = MetadataWarning (MOSource defaultSource) "some warning message"
-- >   warn $ warning
-- >   -- do some more stuff
-- >   pure ()
-- >
module Hasura.RQL.DDL.Warnings
  ( AllowWarnings (..),
    MetadataWarning (..),
    MetadataWarnings,
    MonadWarnings (..),
    runMetadataWarnings,
    mkSuccessResponseWithWarnings,
    successMsgWithWarnings,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Extended ((.=))
import Data.Sequence qualified as Seq
import Hasura.EncJSON (EncJSON, encJFromJValue)
import Hasura.Prelude
import Hasura.RQL.Types.Metadata.Object

{- Note [Warnings in metadata API]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The metadata API handlers return EncJSON, which is just a Bytestring Builder. Now, in order to add warnings to the API
response, we cannot use the `runMetadataWarnings` at the top level (i.e. in `runMetadataQueryM`) as appending something
to the EncJSON will require us to parse the JSON and then re-serialize it. This is wasteful and we should avoid it.

As a result, we are using the `MonadWarnings` class to add warnings at the API handler level, i.e., the API handler will
use the runMetadataWarnings function to run the handler and get the warnings. Then, the API handler will use the warnings
to construct the response.

We can however avoid this by changing the return type of the metadata API handlers to something like:

> data MetadataAPIOutput =
>     RawOutput EncJSON
>   | SuccessWithWarnings MetadataWarnings
>   | InconsistentMetadataWithWarnings MetadataWarnings

This will allow us to cater to the metadata APIs:
- That contacts some external service and passes the raw response (like the export_metadata API).
- That returns a success message with warnings (like the replace_metadata v1 API).
- That returns inconsistent metadata with warnings (like the replace_metadata v2 API).

Also, we can expand the scope of `MetadataAPIOutput` to include other types of responses as well in the future.
-}

-- | Allow/Disallow metadata warnings
data AllowWarnings
  = AllowWarnings
  | NoAllowWarnings
  deriving (Show, Eq)

instance FromJSON AllowWarnings where
  parseJSON =
    Aeson.withBool "AllowWarnings" $
      pure . bool NoAllowWarnings AllowWarnings

instance ToJSON AllowWarnings where
  toJSON = Aeson.toJSON . toBool
    where
      toBool AllowWarnings = True
      toBool NoAllowWarnings = False

data MetadataWarning = MetadataWarning
  { _mwMetadataObj :: MetadataObjId,
    _mwMessage :: Text
  }
  deriving (Eq, Ord)

instance ToJSON MetadataWarning where
  toJSON (MetadataWarning mObj msg) =
    Aeson.object
      [ "message" .= msg,
        "type" .= moiTypeName mObj,
        "name" .= moiName mObj
      ]

type MetadataWarnings = Seq MetadataWarning

class (Monad m) => MonadWarnings m where
  -- | Add a warning to the current context
  warn :: MetadataWarning -> m ()

instance (Monad m) => MonadWarnings (StateT (MetadataWarnings) m) where
  warn w = modify (w Seq.:<|)

runMetadataWarnings :: StateT MetadataWarnings m a -> m (a, MetadataWarnings)
runMetadataWarnings = flip runStateT mempty

mkSuccessResponseWithWarnings :: MetadataWarnings -> EncJSON
mkSuccessResponseWithWarnings warnings =
  encJFromJValue . Aeson.object $
    [ "message" .= ("success" :: Text)
    ]
      <> ["warnings" .= warnings | not (null warnings)]

successMsgWithWarnings :: (Monad m) => (StateT MetadataWarnings m ()) -> m EncJSON
successMsgWithWarnings action = do
  (_, warnings) <- runMetadataWarnings action
  pure $ mkSuccessResponseWithWarnings warnings
