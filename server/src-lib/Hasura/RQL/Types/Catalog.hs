-- | Types that represent the raw data stored in the catalog. See also: the module documentation for
-- "Hasura.RQL.DDL.Schema".
module Hasura.RQL.Types.Catalog
  ( CatalogMetadata(..)

  , CatalogTable(..)
  , CatalogTableInfo(..)

  , CatalogRelation(..)
  , CatalogComputedField(..)
  , CatalogPermission(..)
  , CatalogEventTrigger(..)
  , CatalogFunction(..)
  ) where

import           Hasura.Prelude

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.QueryCollection
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.SchemaCache
import           Hasura.SQL.Types

data CatalogTableInfo
  = CatalogTableInfo
  { _ctiColumns           :: ![PGRawColumnInfo]
  , _ctiConstraints       :: ![ConstraintName]
  , _ctiPrimaryKeyColumns :: ![PGCol]
  , _ctiViewInfo          :: !(Maybe ViewInfo)
  , _ctiDescription       :: !(Maybe PGDescription)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''CatalogTableInfo)

data CatalogTable
  = CatalogTable
  { _ctName            :: !QualifiedTable
  , _ctIsSystemDefined :: !SystemDefined
  , _ctIsEnum          :: !Bool
  , _ctConfiguration   :: !TableConfig
  , _ctInfo            :: !(Maybe CatalogTableInfo)
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

data CatalogComputedField
  = CatalogComputedField
  { _cccComputedField :: !AddComputedField
  , _cccFunctionInfo  :: ![RawFunctionInfo] -- ^ multiple functions with same name
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''CatalogComputedField)

data CatalogEventTrigger
  = CatalogEventTrigger
  { _cetTable :: !QualifiedTable
  , _cetName  :: !TriggerName
  , _cetDef   :: !Value
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''CatalogEventTrigger)

data CatalogFunction
  = CatalogFunction
  { _cfFunction        :: !QualifiedFunction
  , _cfIsSystemDefined :: !SystemDefined
  , _cfConfiguration   :: !FunctionConfig
  , _cfInfo            :: ![RawFunctionInfo] -- ^ multiple functions with same name
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''CatalogFunction)

data CatalogMetadata
  = CatalogMetadata
  { _cmTables               :: ![CatalogTable]
  , _cmRelations            :: ![CatalogRelation]
  , _cmPermissions          :: ![CatalogPermission]
  , _cmEventTriggers        :: ![CatalogEventTrigger]
  , _cmRemoteSchemas        :: ![AddRemoteSchemaQuery]
  , _cmFunctions            :: ![CatalogFunction]
  , _cmForeignKeys          :: ![ForeignKey]
  , _cmAllowlistCollections :: ![CollectionDef]
  , _cmComputedFields       :: ![CatalogComputedField]
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''CatalogMetadata)
