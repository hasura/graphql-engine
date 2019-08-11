-- | This module provides 'fetchCatalogData', which loads the entire catalog in one go from the
-- database, consulting tables such as @hdb_catalog.hdb_table@. It is used by
-- 'Hasura.RQL.Schema.Table.buildSchemaCache' to seed or reload the schema cache.
module Hasura.RQL.Types.Catalog
  ( fetchCatalogData
  , CatalogMetadata(..)

  , CatalogTable(..)
  , CatalogTableInfo(..)

  , CatalogRelation(..)
  , CatalogPermission(..)
  , CatalogEventTrigger(..)
  , CatalogFunction(..)
  ) where

import           Hasura.Prelude

import qualified Database.PG.Query                as Q

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import           Hasura.Db
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
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''CatalogTableInfo)

data CatalogTable
  = CatalogTable
  { _ctName            :: !QualifiedTable
  , _ctIsSystemDefined :: !Bool
  , _ctIsEnum          :: !Bool
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
  , _cmEventTriggers        :: ![CatalogEventTrigger]
  , _cmRemoteSchemas        :: ![AddRemoteSchemaQuery]
  , _cmFunctions            :: ![CatalogFunction]
  , _cmForeignKeys          :: ![ForeignKey]
  , _cmAllowlistCollections :: ![CollectionDef]
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 3 snakeCase) ''CatalogMetadata)

-- | See "Hasura.RQL.Types.Catalog".
fetchCatalogData :: (MonadTx m) => m CatalogMetadata
fetchCatalogData =
  liftTx $ Q.getAltJ . runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
    $(Q.sqlFromFile "src-rsr/catalog_metadata.sql") () True
