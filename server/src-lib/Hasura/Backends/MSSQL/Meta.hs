{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving     #-}

-- |

module Hasura.Backends.MSSQL.Meta
  ( MetadataError(..)
  , loadDBMetadata
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import qualified Data.Text.Encoding          as T

import           Control.Monad.Validate
import           Data.Aeson                  as Aeson
import           Data.Aeson.Types            (parseEither)
import           Data.Attoparsec.ByteString
import           Data.String
import           Database.ODBC.SQLServer     as Odbc

import qualified Hasura.Backends.MSSQL.Types as T

import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common     (Constraint (..), ForeignKey (..), OID (..))
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend


--------------------------------------------------------------------------------
-- Loader

data MetadataError
  = MissingTable T.UserTableName
  | MissingTableForRelationship T.UserUsing
  | MissingColumnForRelationship T.UserUsing
  | UnknownScalarType Text
  deriving (Show)

loadDBMetadata :: Text -> IO (DBTablesMetadata 'MSSQL, [MetadataError])
loadDBMetadata connStr = do
  conn      <- connect connStr
  sql       <- readFile "sql.sql"
  sysTables <- queryJson conn (fromString sql)
  let (errors, tables) = partitionEithers $ transformTable <$> sysTables
  pure (HM.fromList tables, concat errors)


--------------------------------------------------------------------------------
-- Local types

data SysTable = SysTable
  { name              :: Text
  , object_id         :: Int
  , joined_sys_column :: [SysColumn]
  , joined_sys_schema :: SysSchema
  } deriving (Show, Generic)
instance FromJSON SysTable

data SysSchema = SysSchema
  { name      :: Text
  , schema_id :: Int
  } deriving (Show, Generic)
instance FromJSON SysSchema

data SysColumn = SysColumn
  { name                       :: Text
  , column_id                  :: Int
  , system_type_id             :: Int
  , is_nullable                :: Bool
  , joined_sys_type            :: SysType
  , joined_foreign_key_columns :: [SysForeignKeyColumn]
  } deriving (Show, Generic)
instance FromJSON SysColumn

data SysType = SysType
  { name           :: Text
  , schema_id      :: Int
  , system_type_id :: Int
  } deriving (Show, Generic)
instance FromJSON SysType

data SysForeignKeyColumn = SysForeignKeyColumn
  { constraint_object_id          :: Int
  , constraint_column_id          :: Int
  , parent_object_id              :: Int
  , parent_column_id              :: Int
  , referenced_object_id          :: Int
  , referenced_column_id          :: Int
  , joined_referenced_table_name  :: Text
  , joined_referenced_column_name :: Text
  , joined_referenced_sys_schema  :: SysSchema
  } deriving (Show, Generic)
instance FromJSON SysForeignKeyColumn


--------------------------------------------------------------------------------
-- Transform

transformTable :: SysTable -> Either [MetadataError] (T.TableName, DBTableMetadata 'MSSQL)
transformTable tableInfo = runValidate $ do
  let schemaName   = name (joined_sys_schema tableInfo :: SysSchema)
      tableName    = T.TableName (name (tableInfo :: SysTable)) schemaName
      tableOID     = OID $ object_id tableInfo
  (columns, foreignKeys) <- fmap unzip $ traverse transformColumn $ joined_sys_column tableInfo
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
  let prciName        = T.ColumnName $ name (columnInfo :: SysColumn)
      prciPosition    = column_id columnInfo
      -- ^ the IR uses this to uniquely identify columns, as Postgres will
      -- keep a unique position for a column even when columns are added
      -- or dropped. We assume here that this arbitrary column id can
      -- serve the same purpose.
      prciIsNullable  = is_nullable columnInfo
      prciDescription = Nothing
  prciType <- parseScalarType $ name (joined_sys_type columnInfo :: SysType)
  let foreignKeys = joined_foreign_key_columns columnInfo <&> \foreignKeyColumn ->
        let _fkConstraint    = Constraint () {- FIXME -} $ OID $ constraint_object_id foreignKeyColumn
            -- ^ there's currently no ConstraintName type in our MSSQL code?
            schemaName       = name (joined_referenced_sys_schema foreignKeyColumn :: SysSchema)
            _fkForeignTable  = T.TableName (joined_referenced_table_name foreignKeyColumn) schemaName
            _fkColumnMapping = HM.singleton prciName $ T.ColumnName $ joined_referenced_column_name foreignKeyColumn
        in ForeignKey {..}
  pure (RawColumnInfo{..}, foreignKeys)

coalesceKeys :: [ForeignKey 'MSSQL] -> HM.HashMap T.TableName (ForeignKey 'MSSQL)
coalesceKeys = foldl' coalesce HM.empty
  where coalesce mapping fk@(ForeignKey _ tableName _) = HM.insertWith combine tableName fk mapping
        -- is it ok to assume we can coalesce only on table name?
        combine oldFK newFK = oldFK { _fkColumnMapping = (HM.union `on` _fkColumnMapping) oldFK newFK }


parseScalarType :: Text -> Validate [MetadataError] T.ScalarType
parseScalarType = \case
  "int"      -> pure T.IntegerType
  "nvarchar" -> pure T.VarcharType
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
          case parseEither parseJSON jvalue of
            Left e   -> error e
            Right as -> pure as
        Partial {} -> error "Incomplete output from SQL Server."
        Fail _ _ctx err -> error ("JSON parser error: " <> err)


{-
--------------------------------------------------------------------------------
-- Schema loaders

loadMetadata :: FilePath -> IO (Either String UserMetadata)
loadMetadata fp = fmap eitherDecode $ L.readFile fp

loadCatalogMetadata :: Text -> UserMetadata -> IO ()
loadCatalogMetadata connStr UserMetadata{tables} = do
  conn <- connect connStr
  sql <- readFile "sql.sql"
  sysTables :: [SysTable] <- queryJson conn (fromString sql)
  print (runValidate (unifyTables tables sysTables))
  pure ()

--------------------------------------------------------------------------------
-- Unification

unifyTables ::
     [UserTableMetadata]
  -> [SysTable]
  -> Validate (NonEmpty UnifyProblem) [UnifiedTableMetadata]
unifyTables userTableMetadatas sysTables = do
  let indexedSysTables =
        HM.fromList
          (map
             (\sysTable@(SysTable { name
                                  , joined_sys_schema = SysSchema {name = schema}
                                  }) -> (UserTableName {..}, sysTable))
             sysTables)
  tablesStage1 <- traverse (unifyTable indexedSysTables) userTableMetadatas
  let indexedValidTables =
        HM.fromList
          (map
             (\(UserTableMetadata {table}, sysTable) -> (table, sysTable))
             tablesStage1)
  traverse (unifyRelationships indexedValidTables) tablesStage1

unifyTable ::
     HashMap UserTableName SysTable
  -> UserTableMetadata
  -> Validate (NonEmpty UnifyProblem) (UserTableMetadata, SysTable)
unifyTable indexedSysTables userMetaTable@UserTableMetadata {table} = do
  case HM.lookup table indexedSysTables of
    Nothing       -> refute (pure (MissingTable table))
    Just sysTable -> pure (userMetaTable, sysTable)

unifyRelationships ::
     HashMap UserTableName SysTable
  -> (UserTableMetadata, SysTable)
  -> Validate (NonEmpty UnifyProblem) UnifiedTableMetadata
unifyRelationships indexedValidTables (UserTableMetadata {..}, SysTable {joined_sys_column}) = do
  object_relationships' <-
    traverse
      (\UserObjectRelationship {using = using0, ..} -> do
         using <- checkUsing using0
         pure UnifiedObjectRelationship {..})
      object_relationships
  array_relationships' <-
    traverse
      (\UserArrayRelationship {using = using0, ..} -> do
         using <- checkUsing using0
         pure UnifiedArrayRelationship {..})
      array_relationships
  columns <-
    traverse
      (\SysColumn {..} -> do
         let SysType {name = typeName} = joined_sys_type
         type' <- parseScalarType typeName
         pure UnifiedColumn {type', ..})
      joined_sys_column
  pure
    UnifiedTableMetadata
      { table =
          let UserTableName {..} = table
           in UnifiedTableName {..}
      , object_relationships = object_relationships'
      , array_relationships = array_relationships'
      , columns
      }
  where
    checkUsing using@UserUsing {foreign_key_constraint_on} =
      case HM.lookup referencedTable indexedValidTables of
        Nothing -> refute (pure (MissingTableForRelationship using))
        Just SysTable {joined_sys_column = columns} ->
          case find
                 (\SysColumn {name = referencedColumn} ->
                    referencedColumn == column)
                 columns of
            Nothing -> refute (pure (MissingColumnForRelationship using))
            Just {} ->
              pure
                (UnifiedUsing
                   { foreign_key_constraint_on =
                       UnifiedOn
                         { table =
                             let UserTableName {..} = referencedTable
                              in UnifiedTableName {..}
                         , ..
                         }
                   })
      where
        UserOn {table = referencedTable, column} = foreign_key_constraint_on

parseScalarType :: Text -> Validate (NonEmpty UnifyProblem) ScalarType
parseScalarType =
  \case
    "int"      -> pure IntType
    "nvarchar" -> pure NVarCharType
    t          -> refute (pure (UnknownScalarType t))
-}
