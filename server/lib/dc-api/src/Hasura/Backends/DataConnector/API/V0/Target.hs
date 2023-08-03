{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Target
  ( TargetName (..),
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens.TH (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Function qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Table qualified as API.V0
import Prelude

data TargetName
  = TNTable API.V0.TableName
  | TNFunction API.V0.FunctionName
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec TargetName

instance HasCodec TargetName where
  codec = object "TargetName" $ discriminatedUnionCodec "type" enc dec
    where
      tableKey = "table"
      functionKey = "function"
      tnTableObjectCodec = requiredField "table" "The name of the table to query"
      tnFunctionObjectCodec = requiredField "function" "The name of the function"
      enc = \case
        (TNTable t) -> (tableKey, mapToEncoder t tnTableObjectCodec)
        (TNFunction f) -> (functionKey, mapToEncoder f tnFunctionObjectCodec)
      dec =
        HashMap.fromList
          [ (tableKey, ("TNTable", mapToDecoder TNTable tnTableObjectCodec)),
            (functionKey, ("TNFunction", mapToDecoder TNFunction tnFunctionObjectCodec))
          ]

$(makePrisms ''TargetName)
