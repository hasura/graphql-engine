-- | 'get_feature_flag' Metadata API action. Given a feature flag
-- identifier, it returns the value of the flag and its description.
module Hasura.RQL.DDL.FeatureFlag
  ( -- * Get Feature Flag
    GetFeatureFlag (..),
    runGetFeatureFlag,
  )
where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import Data.Aeson (FromJSON, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Hasura.Base.Error qualified as Error
import Hasura.EncJSON (EncJSON)
import Hasura.EncJSON qualified as EncJSON
import Hasura.Prelude
import Hasura.Server.Init.FeatureFlag qualified as FeatureFlag
import Hasura.Server.Types qualified as Types

--------------------------------------------------------------------------------

newtype GetFeatureFlag = GetFeatureFlag {gfgIdentifier :: Text}

instance FromJSON GetFeatureFlag where
  parseJSON = Aeson.withObject "GetFeatureFlag" \o -> do
    gfgIdentifier <- o .: "identifier"
    pure $ GetFeatureFlag {..}

runGetFeatureFlag ::
  ( MonadError Error.QErr m,
    Types.HasServerConfigCtx m,
    MonadIO m
  ) =>
  GetFeatureFlag ->
  m EncJSON
runGetFeatureFlag GetFeatureFlag {..} = do
  checkFeatureFlag <- Types._sccCheckFeatureFlag <$> Types.askServerConfigCtx
  let flagM = HashMap.lookup gfgIdentifier $ FeatureFlag.getFeatureFlags $ FeatureFlag.featureFlags
  case flagM of
    Nothing -> Error.throw400 Error.NotFound $ "Feature Flag '" <> gfgIdentifier <> "' not found"
    Just flag -> do
      flagValue <- liftIO $ checkFeatureFlag flag
      pure $
        EncJSON.encJFromJValue $
          Aeson.object
            [ "identifier" .= gfgIdentifier,
              "value" .= flagValue,
              "description" .= FeatureFlag.ffDescription flag
            ]
