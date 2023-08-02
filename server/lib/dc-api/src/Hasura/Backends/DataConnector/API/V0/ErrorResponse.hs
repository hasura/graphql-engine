{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hasura.Backends.DataConnector.API.V0.ErrorResponse
  ( ErrorResponse (..),
    ErrorResponse400,
    ErrorResponseType (..),
    errorResponseJsonText,
  )
where

import Autodocodec
import Autodocodec.OpenAPI ()
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Text (encodeToLazyText)
import Data.OpenApi (ToSchema (..))
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import GHC.Generics (Generic)
import Servant.API (HasStatus)
import Servant.API.UVerb qualified as Servant
import Prelude

data ErrorResponseType
  = UncaughtError
  | MutationConstraintViolation
  | MutationPermissionCheckFailure
  deriving stock (Eq, Show, Generic)

instance HasCodec ErrorResponseType where
  codec =
    named "ErrorResponseType" $
      stringConstCodec
        [ (UncaughtError, "uncaught-error"),
          (MutationConstraintViolation, "mutation-constraint-violation"),
          (MutationPermissionCheckFailure, "mutation-permission-check-failure")
        ]

data ErrorResponse = ErrorResponse
  { _crType :: ErrorResponseType,
    _crMessage :: Text,
    _crDetails :: J.Value
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ErrorResponse

instance HasStatus ErrorResponse where
  type StatusOf ErrorResponse = 500

type ErrorResponse400 = Servant.WithStatus 400 ErrorResponse

errorResponseJsonText :: ErrorResponse -> Text
errorResponseJsonText = toStrict . encodeToLazyText

instance HasCodec ErrorResponse where
  codec =
    object "ErrorResponse" $
      ErrorResponse
        <$> optionalFieldWithDefault "type" UncaughtError "Error type" .= _crType
        <*> requiredField "message" "Error message" .= _crMessage
        <*> optionalFieldWithDefault "details" J.Null "Error details" .= _crDetails
