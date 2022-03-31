{-# LANGUAGE DeriveAnyClass #-}

--
module Hasura.Backends.DataWrapper.API.V0.Column
  ( ColumnInfo (..),
    ColumnName (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec
import Autodocodec.OpenAPI ()
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.OpenApi (ToSchema)
import Hasura.Backends.DataWrapper.API.V0.Scalar.Type qualified as API.V0.Scalar
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

--------------------------------------------------------------------------------

newtype ColumnName = ColumnName {unColumnName :: Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Cacheable, Hashable)
  deriving newtype (ToJSONKey, FromJSONKey)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ColumnName

instance HasCodec ColumnName where
  codec = dimapCodec ColumnName unColumnName textCodec

--------------------------------------------------------------------------------

data ColumnInfo = ColumnInfo
  { dciName :: ColumnName,
    dciType :: API.V0.Scalar.Type,
    dciNullable :: Bool,
    dciDescription :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Cacheable, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ColumnInfo

instance HasCodec ColumnInfo where
  codec =
    object "ColumnInfo" $
      ColumnInfo
        <$> requiredField "name" "Column name" .= dciName
        <*> requiredField "type" "Column type" .= dciType
        <*> requiredField "nullable" "Is column nullable" .= dciNullable
        <*> optionalFieldOrNull "description" "Column description" .= dciDescription
