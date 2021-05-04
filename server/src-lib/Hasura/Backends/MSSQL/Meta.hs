-- |

module Hasura.Backends.MSSQL.Meta
  ( loadDBMetadata
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                   as HM
import qualified Data.HashSet                          as HS
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import qualified Database.PG.Query                     as Q (sqlFromFile)

import           Data.Aeson                            as Aeson
import           Data.FileEmbed                        (makeRelativeToProject)
import           Data.String

import           Hasura.Backends.MSSQL.Connection
import           Hasura.Backends.MSSQL.Instances.Types ()
import           Hasura.Backends.MSSQL.Types
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common               (OID (..))
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend


--------------------------------------------------------------------------------
-- Loader

loadDBMetadata
  :: (MonadError QErr m, MonadIO m)
  => MSSQLPool -> m (DBTablesMetadata 'MSSQL)
loadDBMetadata pool = do
  let sql = $(makeRelativeToProject "src-rsr/mssql_table_metadata.sql" >>= Q.sqlFromFile)
  sysTablesText <- runJSONPathQuery pool (fromString sql)
  case Aeson.eitherDecodeStrict (T.encodeUtf8 sysTablesText) of
    Left e          -> throw500 $ T.pack $ "error loading sql server database schema: " <> e
    Right sysTables -> pure $ HM.fromList $ map transformTable sysTables


--------------------------------------------------------------------------------
-- Local types

data SysTable = SysTable
  { staName            :: Text
  , staObjectId        :: Int
  , staJoinedSysColumn :: [SysColumn]
  , staJoinedSysSchema :: SysSchema
  } deriving (Show, Generic)

instance FromJSON (SysTable) where
  parseJSON = genericParseJSON hasuraJSON


data SysSchema = SysSchema
  { ssName     :: Text
  , ssSchemaId :: Int
  } deriving (Show, Generic)

instance FromJSON (SysSchema) where
  parseJSON = genericParseJSON hasuraJSON


data SysColumn = SysColumn
  { scName                    :: Text
  , scColumnId                :: Int
  , scUserTypeId              :: Int
  , scIsNullable              :: Bool
  , scJoinedSysType           :: SysType
  , scJoinedForeignKeyColumns :: [SysForeignKeyColumn]
  } deriving (Show, Generic)
instance FromJSON SysColumn where
  parseJSON = genericParseJSON hasuraJSON

data SysType = SysType
  { styName       :: Text
  , stySchemaId   :: Int
  , styUserTypeId :: Int
  } deriving (Show, Generic)

instance FromJSON (SysType) where
  parseJSON = genericParseJSON hasuraJSON


data SysForeignKeyColumn = SysForeignKeyColumn
  { sfkcConstraintObjectId         :: Int
  , sfkcConstraintColumnId         :: Int
  , sfkcParentObjectId             :: Int
  , sfkcParentColumnId             :: Int
  , sfkcReferencedObjectId         :: Int
  , sfkcReferencedColumnId         :: Int
  , sfkcJoinedReferencedTableName  :: Text
  , sfkcJoinedReferencedColumnName :: Text
  , sfkcJoinedReferencedSysSchema  :: SysSchema
  } deriving (Show, Generic)

instance FromJSON (SysForeignKeyColumn) where
  parseJSON = genericParseJSON hasuraJSON


--------------------------------------------------------------------------------
-- Transform

transformTable :: SysTable -> (TableName, DBTableMetadata 'MSSQL)
transformTable tableInfo =
  let schemaName = ssName $ staJoinedSysSchema tableInfo
      tableName  = TableName (staName tableInfo) schemaName
      tableOID   = OID $ staObjectId tableInfo
      (columns, foreignKeys) = unzip $ transformColumn <$> staJoinedSysColumn tableInfo
      foreignKeysMetadata = HS.fromList $ map ForeignKeyMetadata $ coalesceKeys $ concat foreignKeys
  in ( tableName
     , DBTableMetadata
       tableOID
       columns
       Nothing  -- no primary key information?
       HS.empty -- no unique constraints?
       foreignKeysMetadata
       Nothing  -- no views, only tables
       Nothing  -- no description
     )

transformColumn
  :: SysColumn
  -> (RawColumnInfo 'MSSQL, [ForeignKey 'MSSQL])
transformColumn columnInfo =
  let prciName        = ColumnName $ scName columnInfo
      prciPosition    = scColumnId columnInfo
      -- ^ the IR uses this to uniquely identify columns, as Postgres will
      -- keep a unique position for a column even when columns are added
      -- or dropped. We assume here that this arbitrary column id can
      -- serve the same purpose.
      prciIsNullable  = scIsNullable columnInfo
      prciDescription = Nothing
      prciType = parseScalarType $ styName $ scJoinedSysType columnInfo
      foreignKeys = scJoinedForeignKeyColumns columnInfo <&> \foreignKeyColumn ->
        let _fkConstraint    = Constraint () $ OID $ sfkcConstraintObjectId foreignKeyColumn
            -- ^ constraints have no name in MSSQL, and are uniquely identified by their OID
            schemaName       = ssName $ sfkcJoinedReferencedSysSchema foreignKeyColumn
            _fkForeignTable  = TableName (sfkcJoinedReferencedTableName foreignKeyColumn) schemaName
            _fkColumnMapping = HM.singleton prciName $ ColumnName $ sfkcJoinedReferencedColumnName foreignKeyColumn
        in ForeignKey {..}
  in (RawColumnInfo{..}, foreignKeys)


--------------------------------------------------------------------------------
-- Helpers

coalesceKeys :: [ForeignKey 'MSSQL] -> [ForeignKey 'MSSQL]
coalesceKeys = HM.elems . foldl' coalesce HM.empty
  where
    coalesce mapping fk@(ForeignKey constraint tableName _) = HM.insertWith combine (constraint, tableName) fk mapping
    combine oldFK newFK = oldFK { _fkColumnMapping = (HM.union `on` _fkColumnMapping) oldFK newFK }

parseScalarType :: Text -> ScalarType
parseScalarType = \case
  "char"             -> CharType
  "numeric"          -> NumericType
  "decimal"          -> DecimalType
  "money"            -> DecimalType
  "smallmoney"       -> DecimalType
  "int"              -> IntegerType
  "smallint"         -> SmallintType
  "float"            -> FloatType
  "real"             -> RealType
  "date"             -> DateType
  "time"             -> Ss_time2Type
  "varchar"          -> VarcharType
  "nchar"            -> WcharType
  "nvarchar"         -> WvarcharType
  "ntext"            -> WtextType
  "timestamp"        -> TimestampType
  "text"             -> TextType
  "binary"           -> BinaryType
  "bigint"           -> BigintType
  "tinyint"          -> TinyintType
  "varbinary"        -> VarbinaryType
  "bit"              -> BitType
  "uniqueidentifier" -> GuidType
  "geography"        -> GeographyType
  "geometry"         -> GeometryType
  t                  -> UnknownType t
