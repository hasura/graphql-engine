{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Dataset
  ( DatasetTemplateName (..),
    DatasetCloneName (..),
    DatasetGetResponse (..),
    DatasetPostRequest (..),
    DatasetPostResponse (..),
    DatasetDeleteResponse (..),
    datasetGetSuccess,
    datasetDeleteSuccess,
    -- | Lenses
    unDatasetTemplateName,
    unDatasetCloneName,
    dsExists,
    dspFrom,
    dspConfig,
    dsdMessage,
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.ConfigSchema qualified as Config
import Servant.API (FromHttpApiData, ToHttpApiData)
import Prelude

newtype DatasetTemplateName = DatasetTemplateName
  { _unDatasetTemplateName :: Text
  }
  deriving stock (Eq, Ord, Show, Data)
  deriving newtype (ToParamSchema, ToHttpApiData, FromHttpApiData)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec DatasetTemplateName

instance HasCodec DatasetTemplateName where
  codec =
    named "DatasetTemplateName" $
      dimapCodec DatasetTemplateName _unDatasetTemplateName codec

$(makeLenses ''DatasetTemplateName)

newtype DatasetCloneName = DatasetCloneName
  { _unDatasetCloneName :: Text
  }
  deriving stock (Eq, Ord, Show, Data)
  deriving newtype (ToParamSchema, ToHttpApiData, FromHttpApiData)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec DatasetCloneName

instance HasCodec DatasetCloneName where
  codec =
    named "DatasetCloneName" $
      dimapCodec DatasetCloneName _unDatasetCloneName codec

$(makeLenses ''DatasetCloneName)

-- | Request Dataset Info
data DatasetGetResponse = DatasetGetResponse
  { _dsExists :: Bool
  }
  deriving stock (Eq, Ord, Show, Data)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec DatasetGetResponse

datasetGetSuccess :: DatasetGetResponse
datasetGetSuccess = DatasetGetResponse True

instance HasCodec DatasetGetResponse where
  codec =
    object "DatasetGetResponse" $
      DatasetGetResponse
        <$> requiredField "exists" "Message detailing if the dataset exists" .= _dsExists

$(makeLenses ''DatasetGetResponse)

-- | Create a new Dataset
data DatasetPostRequest = DatasetPostRequest
  {_dspFrom :: DatasetTemplateName}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec DatasetPostRequest

instance HasCodec DatasetPostRequest where
  codec =
    object "DatasetPostRequest" $
      DatasetPostRequest
        <$> requiredField "from" "The named dataset to clone from" .= _dspFrom

$(makeLenses ''DatasetPostRequest)

data DatasetPostResponse = DatasetPostResponse
  {_dspConfig :: Config.Config}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec DatasetPostResponse

instance HasCodec DatasetPostResponse where
  codec =
    object "DatasetPostResponse" $
      DatasetPostResponse
        <$> requiredField "config" "A config to connect to the cloned dataset" .= _dspConfig

$(makeLenses ''DatasetPostResponse)

-- | Delete a Dataset
data DatasetDeleteResponse = DatasetDeleteResponse
  { _dsdMessage :: Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec DatasetDeleteResponse

datasetDeleteSuccess :: DatasetDeleteResponse
datasetDeleteSuccess = DatasetDeleteResponse "success"

instance HasCodec DatasetDeleteResponse where
  codec =
    object "DatasetDeleteResponse" $
      DatasetDeleteResponse
        <$> requiredField "message" "The named dataset to clone from" .= _dsdMessage

$(makeLenses ''DatasetDeleteResponse)
