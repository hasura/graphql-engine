{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}

--
module Hasura.Backends.DataConnector.API.V0.OrderBy
  ( OrderBy (..),
    OrderType (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Prelude

--------------------------------------------------------------------------------

data OrderBy = OrderBy
  { column :: API.V0.ColumnName,
    ordering :: OrderType
  }
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec OrderBy

instance HasCodec OrderBy where
  codec =
    object "OrderBy" $
      OrderBy
        <$> requiredField "column" "Column to order by" .= column
        <*> requiredField "ordering" "Ordering" .= ordering

--------------------------------------------------------------------------------

data OrderType
  = Ascending
  | Descending
  deriving stock (Data, Eq, Generic, Ord, Show, Enum, Bounded)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec OrderType

instance HasCodec OrderType where
  codec =
    named "OrderType" $
      disjointStringConstCodec [(Ascending, "asc"), (Descending, "desc")]
