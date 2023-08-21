{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Target
  ( Target (..),
    TargetTable (..),
    TargetFunction (..),
    TargetInterpolatedQuery (..),
    TargetName (..),
    pattern TTargetTable,
    pattern TTargetFunction,
    _TTable,
    _TFunction,
    _TInterpolated,
    ttName,
    tfName,
    tfArguments,
    tiQueryId,
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Function qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.InterpolatedQuery qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Table qualified as API.V0
import Prelude

data Target
  = TTable TargetTable
  | TFunction TargetFunction
  | TInterpolated TargetInterpolatedQuery
  deriving stock (Eq, Ord, Show, Generic, Data)

pattern TTargetTable :: API.V0.TableName -> Target
pattern TTargetTable n = TTable (TargetTable n)

pattern TTargetFunction :: API.V0.FunctionName -> [API.V0.FunctionArgument] -> Target
pattern TTargetFunction n a = TFunction (TargetFunction n a)

instance HasCodec Target where
  codec = object "Target" $ discriminatedUnionCodec "type" enc dec
    where
      tableKey = "table"
      functionKey = "function"
      interpolatedKey = "interpolated"
      enc = \case
        (TTable t) -> (tableKey, mapToEncoder t objectCodec)
        (TFunction f) -> (functionKey, mapToEncoder f objectCodec)
        (TInterpolated i) -> (interpolatedKey, mapToEncoder i objectCodec)
      dec =
        HashMap.fromList
          [ (tableKey, ("TTable", mapToDecoder TTable objectCodec)),
            (functionKey, ("TFunction", mapToDecoder TFunction objectCodec)),
            (interpolatedKey, ("TInterpolated", mapToDecoder TInterpolated objectCodec))
          ]

newtype TargetTable = TargetTable
  { _ttName :: API.V0.TableName
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance HasObjectCodec TargetTable where
  objectCodec = TargetTable <$> requiredField "name" "The name of the table to query" .= _ttName

data TargetFunction = TargetFunction
  { _tfName :: API.V0.FunctionName,
    _tfArguments :: [API.V0.FunctionArgument]
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance HasObjectCodec TargetFunction where
  objectCodec =
    TargetFunction
      <$> requiredField "name" "The name of the function to invoke" .= _tfName
      <*> requiredField "arguments" "The arguments of the function" .= _tfArguments

data TargetInterpolatedQuery = TargetInterpolatedQuery
  { _tiQueryId :: API.V0.InterpolatedQueryId
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance HasObjectCodec TargetInterpolatedQuery where
  objectCodec =
    TargetInterpolatedQuery
      <$> requiredField "id" "The id for the query interpolation template" .= _tiQueryId

data TargetName
  = TNTable API.V0.TableName
  | TNFunction API.V0.FunctionName
  | TNInterpolatedQuery API.V0.InterpolatedQueryId
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec TargetName

instance HasCodec TargetName where
  codec = object "TargetName" $ discriminatedUnionCodec "type" enc dec
    where
      tableKey = "table"
      functionKey = "function"
      interpolatedQueryKey = "interpolated"
      tnTableObjectCodec = requiredField "table" "The name of the table to query"
      tnFunctionObjectCodec = requiredField "function" "The name of the function"
      tnInterpolatedQueryObjectCodec = requiredField "interpolated" "The id of the interpolated query"
      enc = \case
        (TNTable t) -> (tableKey, mapToEncoder t tnTableObjectCodec)
        (TNFunction f) -> (functionKey, mapToEncoder f tnFunctionObjectCodec)
        (TNInterpolatedQuery q) -> (interpolatedQueryKey, mapToEncoder q tnInterpolatedQueryObjectCodec)
      dec =
        HashMap.fromList
          [ (tableKey, ("TNTable", mapToDecoder TNTable tnTableObjectCodec)),
            (functionKey, ("TNFunction", mapToDecoder TNFunction tnFunctionObjectCodec)),
            (interpolatedQueryKey, ("TNInterpolatedQuery", mapToDecoder TNInterpolatedQuery tnInterpolatedQueryObjectCodec))
          ]

$(makePrisms ''Target)
$(makeLenses ''TargetTable)
$(makeLenses ''TargetFunction)
$(makeLenses ''TargetInterpolatedQuery)
$(makePrisms ''TargetName)
