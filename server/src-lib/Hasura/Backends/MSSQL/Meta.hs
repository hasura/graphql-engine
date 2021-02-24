{-# LANGUAGE ApplicativeDo #-}

-- |

module Hasura.Backends.MSSQL.Meta
  ( loadDBMetadata
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                   as HM
import qualified Data.HashSet                          as HS
import qualified Data.Text.Encoding                    as T
import qualified Database.PG.Query                     as Q (sqlFromFile)

import           Data.Aeson                            as Aeson
import           Data.Aeson.Types                      (parseEither)
import           Data.Attoparsec.ByteString
import           Data.String
import           Database.ODBC.SQLServer

import           Hasura.Backends.MSSQL.Instances.Types ()
import           Hasura.Backends.MSSQL.Types
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common               (OID (..))
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend


--------------------------------------------------------------------------------
-- Loader

loadDBMetadata :: Connection -> IO (DBTablesMetadata 'MSSQL)
loadDBMetadata conn = do
  let sql = $(Q.sqlFromFile "src-rsr/mssql_table_metadata.sql")
  sysTables <- queryJson conn (fromString sql)
  let tables = map transformTable sysTables
  pure $ HM.fromList tables

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
  let schemaName   = ssName $ staJoinedSysSchema tableInfo
      tableName    = TableName (staName tableInfo) schemaName
      tableOID     = OID $ staObjectId tableInfo
      (columns, foreignKeys) = unzip $ fmap transformColumn $ staJoinedSysColumn tableInfo
  in ( tableName
     , DBTableMetadata
       tableOID
       columns
       Nothing  -- no primary key information?
       HS.empty -- no unique constraints?
       (HS.fromList $ map ForeignKeyMetadata $ HM.elems $ coalesceKeys $ concat foreignKeys)
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
        let _fkConstraint    = Constraint () {- FIXME -} $ OID $ sfkcConstraintObjectId foreignKeyColumn
            -- ^ there's currently no ConstraintName type in our MSSQL code?
            schemaName       = ssName $ sfkcJoinedReferencedSysSchema foreignKeyColumn
            _fkForeignTable  = TableName (sfkcJoinedReferencedTableName foreignKeyColumn) schemaName
            _fkColumnMapping = HM.singleton prciName $ ColumnName $ sfkcJoinedReferencedColumnName foreignKeyColumn
        in ForeignKey {..}
  in (RawColumnInfo{..}, foreignKeys)


--------------------------------------------------------------------------------
-- Helpers

coalesceKeys :: [ForeignKey 'MSSQL] -> HM.HashMap TableName (ForeignKey 'MSSQL)
coalesceKeys = foldl' coalesce HM.empty
  where coalesce mapping fk@(ForeignKey _ tableName _) = HM.insertWith combine tableName fk mapping
        -- is it ok to assume we can coalesce only on table name?
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


--------------------------------------------------------------------------------
-- Quick catalog queries

queryJson :: FromJSON a => Connection -> Query -> IO [a]
queryJson conn query' = do
  (steps, iresult) <-
    stream
      conn
      query'
      (\(!steps, parser) input ->
         pure (Continue (steps + 1, feed parser (T.encodeUtf8 input))))
      (0 :: Int, parse json mempty)
  case steps of
    0 -> pure []
    _ ->
      case iresult of
        Done _ jvalue ->
          parseEither parseJSON jvalue `onLeft` error -- FIXME
        Partial {} -> error "Incomplete output from SQL Server."
        Fail _ _ctx err -> error ("JSON parser error: " <> err)
