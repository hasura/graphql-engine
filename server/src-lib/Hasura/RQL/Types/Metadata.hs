module Hasura.RQL.Types.Metadata where

import           Data.Aeson
import           Hasura.Prelude

import qualified Data.Text                      as T

import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Types

data TableMetadataObjId
  = MTORel !RelName !RelType
  | MTOComputedField !ComputedFieldName
  | MTOPerm !RoleName !PermType
  | MTOTrigger !TriggerName
  deriving (Show, Eq, Generic)
instance Hashable TableMetadataObjId

data MetadataObjId
  = MOTable !QualifiedTable
  | MOFunction !QualifiedFunction
  | MORemoteSchema !RemoteSchemaName
  | MOTableObj !QualifiedTable !TableMetadataObjId
  deriving (Show, Eq, Generic)
instance Hashable MetadataObjId

data MetadataObject
  = MetadataObject
  { _moId :: !MetadataObjId
  , _moDefinition :: !Value
  } deriving (Show, Eq)

data InconsistentMetadataObj
  = InconsistentMetadataObj
  { _imoObject :: !MetadataObject
  , _imoReason :: !T.Text
  } deriving (Show, Eq)

instance ToJSON InconsistentMetadataObj where
  toJSON (InconsistentMetadataObj (MetadataObject objectId info) rsn) = object
    [ "type" .= String case objectId of
        MOTable _ -> "table"
        MOFunction _ -> "function"
        MORemoteSchema _ -> "remote_schema"
        MOTableObj _ tableObjectId -> case tableObjectId of
          MTORel _ relType -> relTypeToTxt relType <> "_relation"
          MTOPerm _ permType -> permTypeToCode permType <> "_permission"
          MTOTrigger _ -> "event_trigger"
          MTOComputedField _ -> "computed_field"
    , "definition" .= info
    , "reason" .= rsn
    ]
