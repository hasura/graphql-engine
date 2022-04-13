--
module Hasura.Backends.DataWrapper.Adapter.Types
  ( SourceConfig (..),
  )
where

import Data.Aeson qualified as J
import Hasura.Backends.DataWrapper.API.V0.Schema (SchemaResponse)
import Hasura.Incremental (Cacheable (..))
import Hasura.Prelude
import Network.HTTP.Client (Manager)

data SourceConfig = SourceConfig
  { dscEndpoint :: Text,
    dscSchema :: SchemaResponse,
    dscManager :: Manager
  }

instance Show SourceConfig where
  show _ = "SourceConfig"

instance J.ToJSON SourceConfig where
  toJSON _ = J.String "SourceConfig"

instance Eq SourceConfig where
  SourceConfig ep1 schema1 _ == SourceConfig ep2 schema2 _ =
    ep1 == ep2 && schema1 == schema2

instance Cacheable SourceConfig where
  unchanged _ = (==)
