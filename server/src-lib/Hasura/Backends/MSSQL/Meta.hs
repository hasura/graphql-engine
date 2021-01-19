{-# LANGUAGE ApplicativeDo #-}

-- |

module Hasura.Backends.MSSQL.Meta
  ( MetadataError(..)
  , loadDBMetadata
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import qualified Data.Text.Encoding          as T
import qualified Database.PG.Query           as Q (sqlFromFile)

import           Control.Monad.Validate
import           Data.Aeson                  as Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types            (parseEither)
import           Data.Attoparsec.ByteString
import           Data.String
import           Database.ODBC.SQLServer

import           Hasura.Backends.MSSQL.Types
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common     (Constraint (..), ForeignKey (..), OID (..))
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend


--------------------------------------------------------------------------------
-- Loader

data MetadataError
  = UnknownScalarType Text
  deriving (Show)

loadDBMetadata :: Text -> IO (DBTablesMetadata 'MSSQL, [MetadataError])
loadDBMetadata connStr = do
  conn <- connect connStr
  let sql = $(Q.sqlFromFile "src-rsr/mssql_table_metadata.sql")
  sysTables <- queryJson conn (fromString sql)
  let (errors, tables) = partitionEithers $ transformTable <$> sysTables
  pure (HM.fromList tables, concat errors)


--------------------------------------------------------------------------------
-- Local types

data SysTable = SysTable
  { staName            :: Text
  , staObjectId        :: Int
  , staJoinedSysColumn :: [SysColumn]
  , staJoinedSysSchema :: SysSchema
  } deriving (Show, Generic)

instance FromJSON (SysTable) where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)


data SysSchema = SysSchema
  { ssName     :: Text
  , ssSchemaId :: Int
  } deriving (Show, Generic)

instance FromJSON (SysSchema) where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)


data SysColumn = SysColumn
  { scName                    :: Text
  , scColumnId                :: Int
  , scSystemTypeId            :: Int
  , scIsNullable              :: Bool
  , scJoinedSysType           :: SysType
  , scJoinedForeignKeyColumns :: [SysForeignKeyColumn]
  } deriving (Show, Generic)
instance FromJSON SysColumn

data SysType = SysType
  { styName         :: Text
  , stySchemaId     :: Int
  , stySystemTypeId :: Int
  } deriving (Show, Generic)

instance FromJSON (SysType) where
  parseJSON = genericParseJSON (aesonPrefix snakeCase)


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
  parseJSON = genericParseJSON (aesonPrefix snakeCase)


--------------------------------------------------------------------------------
-- Transform

transformTable :: SysTable -> Either [MetadataError] (TableName, DBTableMetadata 'MSSQL)
transformTable tableInfo = runValidate $ do
  let schemaName   = ssName $ staJoinedSysSchema tableInfo
      tableName    = TableName (staName tableInfo) schemaName
      tableOID     = OID $ staObjectId tableInfo
  (columns, foreignKeys) <- fmap unzip $ traverse transformColumn $ staJoinedSysColumn tableInfo
  pure ( tableName
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
  -> Validate [MetadataError] (RawColumnInfo 'MSSQL, [ForeignKey 'MSSQL])
transformColumn columnInfo = do
  let prciName        = ColumnName $ scName columnInfo
      prciPosition    = scColumnId columnInfo
      -- ^ the IR uses this to uniquely identify columns, as Postgres will
      -- keep a unique position for a column even when columns are added
      -- or dropped. We assume here that this arbitrary column id can
      -- serve the same purpose.
      prciIsNullable  = scIsNullable columnInfo
      prciDescription = Nothing
  prciType <- parseScalarType $ styName $ scJoinedSysType columnInfo
  let foreignKeys = scJoinedForeignKeyColumns columnInfo <&> \foreignKeyColumn ->
        let _fkConstraint    = Constraint () {- FIXME -} $ OID $ sfkcConstraintObjectId foreignKeyColumn
            -- ^ there's currently no ConstraintName type in our MSSQL code?
            schemaName       = ssName $ sfkcJoinedReferencedSysSchema foreignKeyColumn
            _fkForeignTable  = TableName (sfkcJoinedReferencedTableName foreignKeyColumn) schemaName
            _fkColumnMapping = HM.singleton prciName $ ColumnName $ sfkcJoinedReferencedColumnName foreignKeyColumn
        in ForeignKey {..}
  pure (RawColumnInfo{..}, foreignKeys)


--------------------------------------------------------------------------------
-- Helpers

coalesceKeys :: [ForeignKey 'MSSQL] -> HM.HashMap TableName (ForeignKey 'MSSQL)
coalesceKeys = foldl' coalesce HM.empty
  where coalesce mapping fk@(ForeignKey _ tableName _) = HM.insertWith combine tableName fk mapping
        -- is it ok to assume we can coalesce only on table name?
        combine oldFK newFK = oldFK { _fkColumnMapping = (HM.union `on` _fkColumnMapping) oldFK newFK }

parseScalarType :: Text -> Validate [MetadataError] ScalarType
parseScalarType = \case
  "int"      -> pure IntegerType
  "nvarchar" -> pure VarcharType
  t          -> refute $ pure (UnknownScalarType t)


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
