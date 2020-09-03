module Hasura.Server.Types where

import           Hasura.Prelude

import           Data.Aeson
import           Language.Haskell.TH.Syntax (Lift)

import qualified Database.PG.Query          as Q

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

-- | The version integer
data APIVersion
  = VIVersion1
  | VIVersion2
  deriving (Show, Eq, Lift)

instance ToJSON APIVersion where
  toJSON VIVersion1 = toJSON @Int 1
  toJSON VIVersion2 = toJSON @Int 2

instance FromJSON APIVersion where
  parseJSON v = do
    verInt :: Int <- parseJSON v
    case verInt of
      1 -> return VIVersion1
      2 -> return VIVersion2
      i -> fail $ "expected 1 or 2, encountered " ++ show i
