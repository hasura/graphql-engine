module Hasura.RQL.Types.Catalog where

import           Hasura.Prelude

import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.QueryCollection
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.SchemaCache
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

data CatalogTable
  = CatalogTable
  { _ctTable         :: !QualifiedTable
  , _ctSystemDefined :: !Bool
  , _ctInfo          :: !(Maybe TableInfo)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''CatalogTable)

data CatalogRelation
  = CatalogRelation
  { _crTable   :: !QualifiedTable
  , _crRelName :: !RelName
  , _crRelType :: !RelType
  , _crDef     :: !Value
  , _crComment :: !(Maybe Text)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''CatalogRelation)

data CatalogPermission
  = CatalogPermission
  { _cpTable    :: !QualifiedTable
  , _cpRole     :: !RoleName
  , _cpPermType :: !PermType
  , _cpDef      :: !Value
  , _cpComment  :: !(Maybe Text)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''CatalogPermission)

data CatalogQueryTemplate
  = CatalogQueryTemplate
  { _cqtName :: !TQueryName
  , _cqtDef  :: !Value
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''CatalogQueryTemplate)

data CatalogEventTrigger
  = CatalogEventTrigger
  { _cetTable :: !QualifiedTable
  , _cetName  :: !TriggerName
  , _cetDef   :: !Value
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''CatalogEventTrigger)

data CatalogFunction
  = CatalogFunction
  { _cfFunction :: !QualifiedFunction
  , _cfInfo     :: !(Maybe RawFuncInfo)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''CatalogFunction)

data CatalogMetadata
  = CatalogMetadata
  { _cmTables               :: ![CatalogTable]
  , _cmRelations            :: ![CatalogRelation]
  , _cmPermissions          :: ![CatalogPermission]
  , _cmQueryTemplates       :: ![CatalogQueryTemplate]
  , _cmEventTriggers        :: ![CatalogEventTrigger]
  , _cmRemoteSchemas        :: ![AddRemoteSchemaQuery]
  , _cmFunctions            :: ![CatalogFunction]
  , _cmForeignKeys          :: ![ForeignKey]
  , _cmAllowlistCollections :: ![CollectionDef]
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''CatalogMetadata)
