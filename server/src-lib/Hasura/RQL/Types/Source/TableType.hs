{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}

module Hasura.RQL.Types.Source.TableType (SourceTableType (..)) where

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Prelude

data SourceTableType = Table | View
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON) via Autodocodec SourceTableType

instance HasCodec SourceTableType where
  codec = named "TableType" (stringConstCodec [(Table, "table"), (View, "view")])
