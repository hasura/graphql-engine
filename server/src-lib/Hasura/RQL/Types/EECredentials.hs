{-# LANGUAGE QuasiQuotes #-}

module Hasura.RQL.Types.EECredentials
  ( EEClientCredentials (..),
    EEClientId (..),
    getEEClientCredentialsTx,
    setEEClientCredentialsTx,
  )
where

import Data.Aeson (FromJSON, (.:))
import Data.Aeson qualified as Aeson
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Base.Error
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

getEEClientCredentialsTx :: PG.TxE QErr (Maybe EEClientCredentials)
getEEClientCredentialsTx =
  makeClientCredentials . PG.getRow
    <$> PG.withQE
      defaultTxErrorHandler
      [PG.sql|
        SELECT ee_client_id::text, ee_client_secret
          FROM hdb_catalog.hdb_version
      |]
      ()
      False
  where
    makeClientCredentials :: (Maybe Text, Maybe Text) -> Maybe EEClientCredentials
    makeClientCredentials (clientIdMaybe, clientSecretMaybe) = do
      eccClientId <- EEClientId <$> clientIdMaybe
      eccClientSecret <- clientSecretMaybe
      pure EEClientCredentials {..}

setEEClientCredentialsTx :: EEClientCredentials -> PG.TxE QErr ()
setEEClientCredentialsTx EEClientCredentials {..} =
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
      UPDATE hdb_catalog.hdb_version
        SET ee_client_id = $1,
            ee_client_secret = $2
    |]
    (_getEEClientId eccClientId, eccClientSecret)
    True
