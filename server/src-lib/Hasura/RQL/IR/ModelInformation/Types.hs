module Hasura.RQL.IR.ModelInformation.Types
  ( ModelType (..),
    ModelSourceType (..),
    ModelOperationType (..),
    ModelNameInfo (..),
    ModelInfoPart (..),
  )
where

import Data.Aeson
import Data.Text.Extended
import Hasura.Prelude
import Hasura.RQL.Types.Common (SourceName)
import Language.GraphQL.Draft.Syntax qualified as G

data ModelType
  = ModelTypeTable
  | ModelTypeLogicalModels
  | ModelTypeFunction
  | ModelTypeIdentifier
  | ModelTypeNativeQuery
  | ModelTypeStoredProcedures
  | ModelTypeRemoteSchema
  | ModelTypeAction
  deriving stock (Generic, Show, Eq)

instance ToTxt ModelType where
  toTxt ModelTypeTable = "table"
  toTxt ModelTypeLogicalModels = "logical_models"
  toTxt ModelTypeFunction = "function"
  toTxt ModelTypeIdentifier = "identifier"
  toTxt ModelTypeNativeQuery = "native_query"
  toTxt ModelTypeStoredProcedures = "stored_procedures"
  toTxt ModelTypeRemoteSchema = "remote_schema"
  toTxt ModelTypeAction = "action"

instance ToJSON ModelType where
  toJSON = String . toTxt

data ModelSourceType
  = ModelSourceTypePostgres
  | ModelSourceTypeMSSQL
  | ModelSourceTypeBigQuery
  | ModelSourceTypeDataConnector
  deriving stock (Generic, Show, Eq)

instance ToTxt ModelSourceType where
  toTxt ModelSourceTypePostgres = "database_postgres"
  toTxt ModelSourceTypeMSSQL = "database_mssql"
  toTxt ModelSourceTypeBigQuery = "database_bigquery"
  toTxt ModelSourceTypeDataConnector = "database_data_connector"

instance ToJSON ModelSourceType where
  toJSON = String . toTxt

newtype ModelOperationType = ModelOperationType {unModelOperationType :: G.OperationType}
  deriving stock (Generic, Show)

instance ToTxt ModelOperationType where
  toTxt (ModelOperationType G.OperationTypeQuery) = "query"
  toTxt (ModelOperationType G.OperationTypeMutation) = "mutation"
  toTxt (ModelOperationType G.OperationTypeSubscription) = "subscription"

instance ToJSON ModelOperationType where
  toJSON = String . toTxt

newtype ModelNameInfo = ModelNameInfo {unModelNameInfo :: (Text, ModelType, SourceName, ModelSourceType)}
  deriving (Show, Eq)

data ModelInfoPart = ModelInfoPart
  { mipModelName :: !Text,
    mipModelType :: !ModelType,
    mipSourceName :: !(Maybe Text),
    mipSourceType :: !(Maybe ModelSourceType),
    mipQueryType :: !ModelOperationType
  }
  deriving stock (Generic, Show)

instance ToJSON ModelInfoPart where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
