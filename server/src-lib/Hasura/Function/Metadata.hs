{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types for storing user-defined-functions in metadata
module Hasura.Function.Metadata
  ( FunctionMetadata (..),
    fmComment,
    fmConfiguration,
    fmFunction,
    fmPermissions,
  )
where

import Autodocodec hiding (object, (.=))
import Autodocodec qualified as AC
import Control.Lens hiding (set, (.=))
import Data.Aeson.Types
import Data.Text qualified as T
import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendPrefix)

data FunctionMetadata b = FunctionMetadata
  { _fmFunction :: FunctionName b,
    _fmConfiguration :: FunctionConfig b,
    _fmPermissions :: [FunctionPermissionInfo],
    _fmComment :: Maybe Text
  }
  deriving (Generic)

deriving instance (Backend b) => Show (FunctionMetadata b)

deriving instance (Backend b) => Eq (FunctionMetadata b)

instance (Backend b) => ToJSON (FunctionMetadata b) where
  toJSON = genericToJSON hasuraJSON

$(makeLenses ''FunctionMetadata)

instance (Backend b) => FromJSON (FunctionMetadata b) where
  parseJSON = withObject "FunctionMetadata" $ \o ->
    FunctionMetadata
      <$> o
      .: "function"
      <*> o
      .:? "configuration"
      .!= emptyFunctionConfig
      <*> o
      .:? "permissions"
      .!= []
      <*> o
      .:? "comment"

instance (Backend b) => HasCodec (FunctionMetadata b) where
  codec =
    CommentCodec
      ( T.unlines
          [ "A custom SQL function to add to the GraphQL schema with configuration.",
            "",
            "https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-functions.html#args-syntax"
          ]
      )
      $ AC.object (backendPrefix @b <> "FunctionMetadata")
      $ FunctionMetadata
      <$> requiredField "function" nameDoc
      AC..= _fmFunction
        <*> optionalFieldWithOmittedDefault "configuration" emptyFunctionConfig configDoc
      AC..= _fmConfiguration
        <*> optionalFieldWithOmittedDefault' "permissions" []
      AC..= _fmPermissions
        <*> optionalField' "comment"
      AC..= _fmComment
    where
      nameDoc = "Name of the SQL function"
      configDoc = "Configuration for the SQL function"
