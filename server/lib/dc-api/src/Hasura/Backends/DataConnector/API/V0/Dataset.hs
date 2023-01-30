{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Dataset
  ( DatasetTemplateName (..),
    unDatasetTemplateName,
    DatasetCloneName (..),
    unDatasetCloneName,
    DatasetGetTemplateResponse (..),
    dgtrExists,
    datasetGetTemplateSuccess,
    DatasetCreateCloneRequest (..),
    dccrFrom,
    DatasetCreateCloneResponse (..),
    dccrConfig,
    DatasetDeleteCloneResponse (..),
    ddcrMessage,
    datasetDeleteCloneSuccess,
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
data DatasetGetTemplateResponse = DatasetGetTemplateResponse
  { _dgtrExists :: Bool
  }
  deriving stock (Eq, Ord, Show, Data)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec DatasetGetTemplateResponse

datasetGetTemplateSuccess :: DatasetGetTemplateResponse
datasetGetTemplateSuccess = DatasetGetTemplateResponse True

instance HasCodec DatasetGetTemplateResponse where
  codec =
    object "DatasetGetTemplateResponse" $
      DatasetGetTemplateResponse
        <$> requiredField "exists" "Message detailing if the dataset exists" .= _dgtrExists

$(makeLenses ''DatasetGetTemplateResponse)

-- | Create a new Dataset
data DatasetCreateCloneRequest = DatasetCreateCloneRequest
  {_dccrFrom :: DatasetTemplateName}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec DatasetCreateCloneRequest

instance HasCodec DatasetCreateCloneRequest where
  codec =
    object "DatasetCreateCloneRequest" $
      DatasetCreateCloneRequest
        <$> requiredField "from" "The named dataset to clone from" .= _dccrFrom

$(makeLenses ''DatasetCreateCloneRequest)

data DatasetCreateCloneResponse = DatasetCreateCloneResponse
  {_dccrConfig :: Config.Config}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec DatasetCreateCloneResponse

instance HasCodec DatasetCreateCloneResponse where
  codec =
    object "DatasetCreateCloneResponse" $
      DatasetCreateCloneResponse
        <$> requiredField "config" "A config to connect to the cloned dataset" .= _dccrConfig

$(makeLenses ''DatasetCreateCloneResponse)

-- | Delete a Dataset
data DatasetDeleteCloneResponse = DatasetDeleteCloneResponse
  { _ddcrMessage :: Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec DatasetDeleteCloneResponse

datasetDeleteCloneSuccess :: DatasetDeleteCloneResponse
datasetDeleteCloneSuccess = DatasetDeleteCloneResponse "success"

instance HasCodec DatasetDeleteCloneResponse where
  codec =
    object "DatasetDeleteCloneResponse" $
      DatasetDeleteCloneResponse
        <$> requiredField "message" "The named dataset to clone from" .= _ddcrMessage

$(makeLenses ''DatasetDeleteCloneResponse)
