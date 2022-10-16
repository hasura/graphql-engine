{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hasura.Backends.DataConnector.API.V0.ErrorResponse
  ( ErrorResponse (..),
    ErrorResponseType (..),
    errorResponseJsonText,
    errorResponseSummary,
  )
where

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (first)
import Data.Data (Data, Proxy (..))
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (mapMaybe)
import Data.OpenApi (NamedSchema (..), OpenApiType (OpenApiObject, OpenApiString), Referenced (..), Schema (..), ToSchema (..), declareSchemaRef)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import GHC.Generics (Generic)
import Servant.API (HasStatus)
import Servant.API.UVerb qualified as Servant
import Prelude

data ErrorResponseType
  = UncaughtError
  deriving stock (Eq, Show, Generic)

instance HasCodec ErrorResponseType where
  codec =
    named "ErrorResponseType" $
      stringConstCodec [(UncaughtError, "uncaught-error")]

data ErrorResponse = ErrorResponse
  { _crType :: ErrorResponseType,
    _crMessage :: Text,
    _crDetails :: J.Value
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ErrorResponse

instance HasStatus ErrorResponse where
  type StatusOf ErrorResponse = 500

{-# HLINT ignore "Use tshow" #-}
errorResponseSummary :: ErrorResponse -> Text
errorResponseSummary ErrorResponse {..} = pack (show _crType) <> ": " <> _crMessage

errorResponseJsonText :: ErrorResponse -> Text
errorResponseJsonText = toStrict . encodeToLazyText

instance HasCodec ErrorResponse where
  codec =
    object "ErrorResponse" $
      ErrorResponse
        <$> optionalFieldWithDefault "type" UncaughtError "Error type" .= _crType
        <*> requiredField "message" "Error message" .= _crMessage
        <*> optionalFieldWithDefault "details" J.Null "Error details" .= _crDetails
