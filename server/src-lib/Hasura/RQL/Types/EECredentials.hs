module Hasura.RQL.Types.EECredentials
  ( EEClientCredentials (..),
    EEClientId (..),
  )
where

import Data.Aeson (FromJSON, (.:))
import Data.Aeson qualified as Aeson
import Hasura.Prelude

data EEClientCredentials = EEClientCredentials
  { eccClientId :: EEClientId,
    eccClientSecret :: Text
  }

newtype EEClientId = EEClientId {_getEEClientId :: Text}
  deriving newtype (FromJSON)

instance FromJSON EEClientCredentials where
  parseJSON = Aeson.withObject "EEClientCredentials" $ \o -> do
    eccClientId <- o .: "client_id"
    eccClientSecret <- o .: "client_secret"
    pure EEClientCredentials {..}
