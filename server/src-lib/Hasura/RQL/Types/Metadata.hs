module Hasura.RQL.Types.Metadata where

import           Data.Aeson
import           Hasura.Prelude

import qualified Data.Text                     as T

import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Types

data MetadataObjType
  = MOTTable
  | MOTRel !RelType
  | MOTPerm !PermType
  | MOTEventTrigger
  | MOTFunction
  | MOTRemoteSchema
  deriving (Eq)

instance Show MetadataObjType where
  show MOTTable        = "table"
  show (MOTRel ty)     = T.unpack (relTypeToTxt ty) <> "_relation"
  show (MOTPerm ty)    = show ty <> "_permission"
  show MOTEventTrigger = "event_trigger"
  show MOTFunction     = "function"
  show MOTRemoteSchema = "remote_schema"

instance ToJSON MetadataObjType where
  toJSON = String . T.pack . show

data TableMetadataObjId
  = MTORel !RelName !RelType
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

data InconsistentMetadataObj
  = InconsistentMetadataObj
  { _moId     :: !MetadataObjId
  , _moType   :: !MetadataObjType
  , _moDef    :: !Value
  , _moReason :: !T.Text
  } deriving (Show, Eq)

instance ToJSON InconsistentMetadataObj where
  toJSON (InconsistentMetadataObj _ ty info rsn) =
    object [ "type" .= ty
           , "definition" .= info
           , "reason" .= rsn
           ]
