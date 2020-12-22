-- TODO (Karthikeyan): This file should be removed. This file hasn't been removed to help with the
-- conflict resolution
-- | Types that represent the raw data stored in the catalog. See also: the module documentation for
-- "Hasura.RQL.DDL.Schema".
module Hasura.RQL.Types.Catalog
  ( CatalogMetadata(..)

  , CatalogTable(..)
  , CatalogTableInfo(..)
  , CatalogForeignKey(..)

  , CatalogRelation(..)
  , CatalogComputedField(..)
  , CatalogPermission(..)
  , CatalogEventTrigger(..)
  , CatalogFunction(..)
  , CatalogCronTrigger(..)
  , CatalogCustomTypes(..)
  , CatalogRemoteSchemaPermission
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as M

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           System.Cron.Types                   (CronSchedule (..))

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Incremental                  (Cacheable)
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.QueryCollection
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.ScheduledTrigger
import           Hasura.RQL.Types.SchemaCache
import           Hasura.SQL.Backend
import           Hasura.Session

newtype CatalogForeignKey
  = CatalogForeignKey
  { unCatalogForeignKey :: ForeignKey
  } deriving (Show, Eq, NFData, Hashable, Cacheable)

instance FromJSON CatalogForeignKey where
  parseJSON = withObject "CatalogForeignKey" \o -> do
    constraint <- o .: "constraint"
    foreignTable <- o .: "foreign_table"

    columns <- o .: "columns"
    foreignColumns <- o .: "foreign_columns"
    unless (length columns == length foreignColumns) $
      fail "columns and foreign_columns differ in length"

    pure $ CatalogForeignKey ForeignKey
      { _fkConstraint = constraint
      , _fkForeignTable = foreignTable
      , _fkColumnMapping = M.fromList $ zip columns foreignColumns
      }

data CatalogTableInfo
  = CatalogTableInfo
  { _ctiOid               :: !OID
  , _ctiColumns           :: ![RawColumnInfo 'Postgres]
  , _ctiPrimaryKey        :: !(Maybe (PrimaryKey PGCol))
  , _ctiUniqueConstraints :: !(HashSet Constraint)
  -- ^ Does /not/ include the primary key!
  , _ctiForeignKeys       :: !(HashSet CatalogForeignKey)
  , _ctiViewInfo          :: !(Maybe ViewInfo)
  , _ctiDescription       :: !(Maybe PGDescription)
  } deriving (Eq, Generic)
instance NFData CatalogTableInfo
instance Cacheable CatalogTableInfo
$(deriveFromJSON (aesonDrop 4 snakeCase) ''CatalogTableInfo)

data CatalogTable
  = CatalogTable
  { _ctName            :: !QualifiedTable
  , _ctIsSystemDefined :: !SystemDefined
  , _ctIsEnum          :: !Bool
  , _ctConfiguration   :: !TableConfig
  , _ctInfo            :: !(Maybe CatalogTableInfo)
  } deriving (Eq, Generic)
instance NFData CatalogTable
instance Cacheable CatalogTable
$(deriveFromJSON (aesonDrop 3 snakeCase) ''CatalogTable)

data CatalogRelation
  = CatalogRelation
  { _crTable   :: !QualifiedTable
  , _crRelName :: !RelName
  , _crRelType :: !RelType
  , _crDef     :: !Value
  , _crComment :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance NFData CatalogRelation
instance Cacheable CatalogRelation
$(deriveFromJSON (aesonDrop 3 snakeCase) ''CatalogRelation)

data CatalogPermission
  = CatalogPermission
  { _cpTable    :: !QualifiedTable
  , _cpRole     :: !RoleName
  , _cpPermType :: !PermType
  , _cpDef      :: !Value
  , _cpComment  :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance NFData CatalogPermission
instance Hashable CatalogPermission
instance Cacheable CatalogPermission
$(deriveFromJSON (aesonDrop 3 snakeCase) ''CatalogPermission)

data CatalogComputedField
  = CatalogComputedField
  { _cccComputedField :: !AddComputedField
  , _cccFunctionInfo  :: ![RawFunctionInfo] -- ^ multiple functions with same name
  } deriving (Show, Eq, Generic)
instance NFData CatalogComputedField
instance Cacheable CatalogComputedField
$(deriveFromJSON (aesonDrop 4 snakeCase) ''CatalogComputedField)

data CatalogEventTrigger
  = CatalogEventTrigger
  { _cetTable :: !QualifiedTable
  , _cetName  :: !TriggerName
  , _cetDef   :: !Value
  } deriving (Show, Eq, Generic)
instance NFData CatalogEventTrigger
instance Cacheable CatalogEventTrigger
$(deriveFromJSON (aesonDrop 4 snakeCase) ''CatalogEventTrigger)

data CatalogFunction
  = CatalogFunction
  { _cfFunction        :: !QualifiedFunction
  , _cfIsSystemDefined :: !SystemDefined
  , _cfConfiguration   :: !FunctionConfig
  , _cfInfo            :: ![RawFunctionInfo] -- ^ multiple functions with same name
  } deriving (Show, Eq, Generic)
instance NFData CatalogFunction
instance Cacheable CatalogFunction
$(deriveFromJSON (aesonDrop 3 snakeCase) ''CatalogFunction)

data CatalogCustomTypes (b :: BackendType)
  = CatalogCustomTypes
  { _cctCustomTypes :: !CustomTypes
  , _cctPgScalars   :: !(HashSet (ScalarType b))
  -- ^ All Postgres base types, which may be referenced in custom type definitions.
  -- When we validate the custom types (see 'validateCustomTypeDefinitions'),
  -- we record which base types were referenced so that we can be sure to include them
  -- in the generated GraphQL schema.
  --
  -- These are not actually part of the Hasura metadata --- we fetch them from
  -- @pg_catalog.pg_type@ --- but they’re needed when validating the custom type
  -- metadata, so we include them here.
  --
  -- See Note [Postgres scalars in custom types] for more details.
  } deriving (Generic)
instance NFData (CatalogCustomTypes 'Postgres)
deriving instance Eq (CatalogCustomTypes 'Postgres)
instance Cacheable (CatalogCustomTypes 'Postgres)
instance FromJSON (CatalogCustomTypes 'Postgres) where
  parseJSON = genericParseJSON $ aesonDrop 4 snakeCase

type CatalogAction = ActionMetadata

data CatalogCronTrigger
  = CatalogCronTrigger
  { _cctName         :: !TriggerName
  , _cctWebhookConf  :: !InputWebhook
  , _cctCronSchedule :: !CronSchedule
  , _cctPayload      :: !(Maybe Value)
  , _cctRetryConf    :: !(Maybe STRetryConf)
  , _cctHeaderConf   :: !(Maybe [HeaderConf])
  , _cctComment      :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance NFData CatalogCronTrigger
instance Cacheable CatalogCronTrigger
$(deriveJSON (aesonDrop 4 snakeCase) ''CatalogCronTrigger)

type CatalogRemoteSchemaPermission = AddRemoteSchemaPermissions

data CatalogMetadata
  = CatalogMetadata
  { _cmTables                  :: ![CatalogTable]
  , _cmRelations               :: ![CatalogRelation]
  , _cmPermissions             :: ![CatalogPermission]
  , _cmEventTriggers           :: ![CatalogEventTrigger]
  , _cmRemoteSchemas           :: ![AddRemoteSchemaQuery]
  , _cmFunctions               :: ![CatalogFunction]
  , _cmAllowlistCollections    :: ![CollectionDef]
  , _cmComputedFields          :: ![CatalogComputedField]
  , _cmCustomTypes             :: !(CatalogCustomTypes 'Postgres)
  , _cmActions                 :: ![CatalogAction]
  , _cmRemoteRelationships     :: ![RemoteRelationship]
  , _cmCronTriggers            :: ![CatalogCronTrigger]
  , _cmRemoteSchemaPermissions :: ![CatalogRemoteSchemaPermission]
  } deriving (Eq, Generic)
instance NFData CatalogMetadata
instance Cacheable CatalogMetadata
instance FromJSON CatalogMetadata where
  parseJSON = genericParseJSON $ aesonDrop 3 snakeCase
