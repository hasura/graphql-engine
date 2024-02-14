{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Core representation of FunctionRequests and related types.
module Hasura.Backends.DataConnector.API.V0.Function
  ( FunctionName (..),
    FunctionInfo (..),
    FunctionType (..),
    FunctionArg (..),
    FunctionArgument (..),
    ArgumentValue (..),
    FunctionReturnType (..),
    FunctionArity (..),
    functionNameToText,
    fiDescription,
    fiFunctionType,
    fiInputArgs,
    fiName,
    fiReturns,
    fiResponseCardinality,
    faInputArgOptional,
    faInputArgName,
    faInputArgType,
    faName,
    faValue,
    savValue,
    _FunctionReturnsTable,
    _FunctionReturnsUnknown,
  )
where

--------------------------------------------------------------------------------

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable ())
import Data.List.NonEmpty qualified as NonEmpty
import Data.OpenApi (ToSchema)
import Data.Text (Text, intercalate)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Scalar qualified as API
import Hasura.Backends.DataConnector.API.V0.Table qualified as API
import Witch qualified
import Prelude

--------------------------------------------------------------------------------

-- | The fully qualified name of a function. The last element in the list is the function name
-- and all other elements represent namespacing of the function name.
-- Matches the structure of TableName.
newtype FunctionName = FunctionName {unFunctionName :: NonEmpty.NonEmpty Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec FunctionName

-- | Encode a text representation of a function name for display purposes
--   Note, this loses some fidelity due to not being a bidirectional encoding
--   so should only be used for display/logging purposes, not transport.
functionNameToText :: FunctionName -> Text
functionNameToText (FunctionName tns) = intercalate "." (NonEmpty.toList tns)

instance Witch.From (NonEmpty.NonEmpty Text) FunctionName

instance HasCodec FunctionName where
  codec =
    named "FunctionName" $
      dimapCodec FunctionName unFunctionName codec
        <?> "The fully qualified name of a function, where the last item in the array is the function name and any earlier items represent the namespacing of the function name"

--------------------------------------------------------------------------------

data FunctionType = FRead | FWrite
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec FunctionType

instance HasCodec FunctionType where
  codec = named "FunctionType" $ stringConstCodec [(FRead, "read"), (FWrite, "write")]

-- | FunctionArg represents the args exposed in the agent schema
--
-- TODO: This should be extended to support positional args, etc. in future
-- Example: `data FunctionArgIdentifier = NamedArg Text | PositionalArg Int`
-- Serialized: `{ "type": "name", "name": "arg1" }` or `{ "type": "positional", "index": 0 }`
data FunctionArg = FunctionArg
  { _faInputArgName :: Text,
    _faInputArgType :: API.ScalarType,
    _faInputArgOptional :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec FunctionArg

instance HasCodec FunctionArg where
  codec =
    object "FunctionInformationArgument" $
      FunctionArg
        <$> requiredField "name" "The name of the argument" .= _faInputArgName
        <*> requiredField "type" "The type of the argument" .= _faInputArgType
        <*> optionalFieldWithDefault "optional" False "If the argument can be omitted" .= _faInputArgOptional

data FunctionReturnType
  = FunctionReturnsTable API.TableName
  | FunctionReturnsUnknown
  -- TODO: Integrate Logical Model support
  -- TODO: Integrate Scalar support
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec FunctionReturnType

-- This is very similar to the definitions for tableFunctionResponseCodec and FunctionResponse
-- in server/src-lib/Hasura/Function/Cache.hs and server/src-lib/Hasura/RQL/Types/Backend.hs
instance HasCodec FunctionReturnType where
  codec =
    named "FunctionReturnType" $
      object "FunctionReturnType" $
        discriminatedUnionCodec "type" enc dec
    where
      enc = \case
        FunctionReturnsTable rt -> ("table", mapToEncoder rt (FunctionReturnsTable <$> requiredField' "table"))
        FunctionReturnsUnknown -> ("unknown", mapToEncoder () (pureCodec FunctionReturnsUnknown))
      dec =
        HashMap.fromList
          [ ("table", ("FunctionReturnsTable", mapToDecoder FunctionReturnsTable (requiredField' "table"))),
            ("unknown", ("FunctionReturnsUnknown", pure FunctionReturnsUnknown))
          ]

-- | FunctionArgument represent arguments sent via Request IR to the agent.
data FunctionArgument = NamedArgument
  { _faName :: Text,
    _faValue :: ArgumentValue
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

-- | This type wrapper exists to allow easy extension to different types in the future.
newtype ArgumentValue = ScalarArgumentValue
  { _savValue :: API.ScalarValue
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance HasCodec ArgumentValue where
  codec =
    object "ArgumentValue" $
      discriminatedUnionCodec "type" enc dec
    where
      enc = \case
        (ScalarArgumentValue n) -> ("scalar", mapToEncoder n objectCodec)
      dec =
        HashMap.fromList
          [ ("scalar", ("ScalarArgumentValue", mapToDecoder ScalarArgumentValue objectCodec))
          ]

namedArgumentObjectCodec :: JSONObjectCodec FunctionArgument
namedArgumentObjectCodec =
  NamedArgument
    <$> requiredField "name" "The name of the named argument" .= _faName
    <*> requiredField "value" "The value of the named argument" .= _faValue

instance HasCodec FunctionArgument where
  codec =
    object "FunctionRequestArgument" $
      discriminatedUnionCodec "type" enc dec
    where
      enc = \case
        n -> ("named", mapToEncoder n namedArgumentObjectCodec)
      dec =
        HashMap.fromList
          [ ("named", ("NamedArgument", mapToDecoder id namedArgumentObjectCodec))
          ]

data FunctionArity = FunctionArityOne | FunctionArityMany
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance HasCodec FunctionArity where
  codec =
    stringConstCodec
      [ (FunctionArityOne, "one"),
        (FunctionArityMany, "many")
      ]

-- | Function schema data from the 'SchemaResponse'. -- TODO: Adapt this to more closely match: PGRawFunctionInfo
--  Just mostly reuse PGRawFunctionInfo for now, and refine as we think of how to refine it.
--  May want to represent this much more like a GraphQL definition.
data FunctionInfo = FunctionInfo
  { -- NOTE: Some fields from PG are omitted here due to initial implementation, or non-generality.
    _fiName :: FunctionName,
    _fiFunctionType :: FunctionType,
    _fiReturns :: Maybe FunctionReturnType, -- Functions must currently return tables as per PG.
    _fiResponseCardinality :: Maybe FunctionArity,
    _fiInputArgs :: [FunctionArg], -- Args info is listed grouped unlike PG.
    _fiDescription :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec FunctionInfo

instance HasCodec FunctionInfo where
  codec =
    object "FunctionInfo" $
      FunctionInfo
        <$> requiredField "name" "The name of the table" .= _fiName
        <*> requiredField "type" "read/write classification of the function" .= _fiFunctionType
        <*> optionalFieldOrNull "returns" "table listed in schema that matches the return type of the function - to relax later" .= _fiReturns
        <*> optionalFieldOrNull "response_cardinality" "object response if false, rows if true" .= _fiResponseCardinality
        <*> optionalFieldWithOmittedDefault "args" [] "argument info - name/types" .= _fiInputArgs
        <*> optionalFieldOrNull "description" "Description of the table" .= _fiDescription

--------------------------------------------------------------------------------

$(makeLenses ''FunctionInfo)
$(makeLenses ''FunctionArg)
$(makeLenses ''FunctionArgument)
$(makeLenses ''ArgumentValue)
$(makePrisms ''FunctionReturnType)
$(makePrisms ''FunctionArity)
$(makePrisms ''FunctionType)
