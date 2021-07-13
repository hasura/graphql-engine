{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Hasura.Backends.MySQL.Types where


import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J
import           Data.Hashable
import           Data.Pool
import           Data.Word                              (Word16)
import           Database.MySQL.Base
import           Hasura.Incremental.Internal.Dependency (Cacheable (..))
import           Hasura.Prelude
import           System.IO.Unsafe                       (unsafePerformIO)


-- | Partial of Database.MySQL.Simple.ConnectInfo
data ConnSourceConfig
  = ConnSourceConfig
    { _cscHost     :: !Text -- ^ Works with @127.0.0.1@ but not with @localhost@ for some reason
    , _cscPort     :: !Word16
    , _cscUser     :: !Text
    , _cscPassword :: !Text
    , _cscDatabase :: !Text
    } deriving (Eq, Show, NFData, Generic, Hashable)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) {J.omitNothingFields = False} ''ConnSourceConfig)
instance Arbitrary ConnSourceConfig where
  arbitrary = genericArbitrary
instance Cacheable Word16 where
  unchanged _ = (==)
deriving instance Cacheable ConnSourceConfig


data SourceConfig
  = SourceConfig
    { scConfig         :: !ConnSourceConfig
    , scConnectionPool :: !(Pool Connection)
    } deriving (Eq, Generic, J.ToJSON)
instance Arbitrary SourceConfig where
  arbitrary = genericArbitrary
instance J.ToJSON (Pool Connection) where
  toJSON = const (J.String "_REDACTED_")
instance Eq (Pool Connection) where
  _ == _ = True
instance Arbitrary (Pool Connection) where
  arbitrary = pure . unsafePerformIO $
    createPool (connect defaultConnectInfo) close 1 (60 {-seconds-} * 60 {-minutes-}) 1
instance Cacheable SourceConfig where
  unchanged _ = (==)


