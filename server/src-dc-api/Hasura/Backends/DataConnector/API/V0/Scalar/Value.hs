{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Scalar.Value
  ( Value (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens ((^?))
import Control.Lens.TH (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

--------------------------------------------------------------------------------

data Value
  = String Text
  | Number Scientific
  | Boolean Bool
  | Null
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)

$(makePrisms ''Value)

instance HasCodec Value where
  codec =
    named "ScalarValue" $
      disjointMatchChoicesNECodec
        [ DisjunctCodec (^? _String) (String <$> codec @Text),
          DisjunctCodec (^? _Number) (Number <$> codec @Scientific),
          DisjunctCodec (^? _Boolean) (Boolean <$> codec @Bool),
          DisjunctCodec (^? _Null) (Null <$ nullCodec)
        ]

deriving via Autodocodec Value instance FromJSON Value

deriving via Autodocodec Value instance ToJSON Value

deriving via Autodocodec Value instance ToSchema Value
