module Hasura.Server.Types where

import           Hasura.Prelude

import           Data.Aeson

import qualified Database.PG.Query as Q

newtype RequestId
  = RequestId { unRequestId :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

newtype DbUid
  = DbUid { getDbUid :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

newtype PGVersion
  = PGVersion { unPGVersion :: Int }
  deriving (Show, Eq, ToJSON)

newtype InstanceId
  = InstanceId { getInstanceId :: Text }
  deriving (Show, Eq, ToJSON, FromJSON, Q.FromCol, Q.ToPrepArg)
