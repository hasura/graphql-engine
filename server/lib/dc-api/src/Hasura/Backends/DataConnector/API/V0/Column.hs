{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Column
  ( ColumnInfo (..),
    ciName,
    ciType,
    ciNullable,
    ciDescription,
    ciInsertable,
    ciUpdatable,
    ColumnName (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Scalar qualified as API.V0.Scalar
import Prelude

--------------------------------------------------------------------------------

newtype ColumnName = ColumnName {unColumnName :: Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving newtype (ToJSONKey, FromJSONKey)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ColumnName

instance HasCodec ColumnName where
  codec = dimapCodec ColumnName unColumnName textCodec

--------------------------------------------------------------------------------

data ColumnInfo = ColumnInfo
  { _ciName :: ColumnName,
    _ciType :: API.V0.Scalar.ScalarType,
    _ciNullable :: Bool,
    _ciDescription :: Maybe Text,
    _ciInsertable :: Bool,
    _ciUpdatable :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ColumnInfo

instance HasCodec ColumnInfo where
  codec =
    object "ColumnInfo" $
      ColumnInfo
        <$> requiredField "name" "Column name" .= _ciName
        <*> requiredField "type" "Column type" .= _ciType
        <*> requiredField "nullable" "Is column nullable" .= _ciNullable
        <*> optionalFieldOrNull "description" "Column description" .= _ciDescription
        <*> optionalFieldWithDefault "insertable" False "Whether or not the column can be inserted into" .= _ciInsertable
        <*> optionalFieldWithDefault "updatable" False "Whether or not the column can be updated" .= _ciUpdatable

$(makeLenses ''ColumnInfo)
