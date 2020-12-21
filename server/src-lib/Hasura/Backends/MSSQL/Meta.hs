{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ApplicativeDo #-}
{-# language DuplicateRecordFields #-}

-- |

module Hasura.Backends.MSSQL.Meta where

import           Control.Monad.Validate
import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseEither)
import           Data.Attoparsec.ByteString
import qualified Data.ByteString.Lazy as L
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import           Data.String
import qualified Data.Text.Encoding as T
import           Database.ODBC.SQLServer as Odbc
import           Hasura.Backends.MSSQL.Types
import           Hasura.Prelude

--------------------------------------------------------------------------------
-- Types

data SysTable = SysTable
  { name :: Text
  , object_id :: Int
  , joined_sys_column :: [SysColumn]
  , joined_sys_schema :: SysSchema
  } deriving (Show, Generic)
instance FromJSON SysTable

data SysSchema = SysSchema
  { name :: Text
  , schema_id :: Int
  } deriving (Show, Generic)
instance FromJSON SysSchema

data SysColumn = SysColumn
  { name :: Text
  , column_id :: Int
  , system_type_id :: Int
  , is_nullable :: Bool
  , joined_sys_type :: SysType
  , joined_foreign_key_columns :: [SysForeignKeyColumn]
  } deriving (Show, Generic)
instance FromJSON SysColumn

data SysType = SysType
  { name :: Text
  , schema_id :: Int
  , system_type_id :: Int
  } deriving (Show, Generic)
instance FromJSON SysType

data SysForeignKeyColumn = SysForeignKeyColumn
  { constraint_object_id :: Int
  , constraint_column_id :: Int
  , parent_object_id :: Int
  , parent_column_id :: Int
  , referenced_object_id :: Int
  , referenced_column_id :: Int
  , joined_referenced_table_name :: Text
  , joined_referenced_column_name :: Text
  , joined_referenced_sys_schema :: SysSchema
  } deriving (Show, Generic)
instance FromJSON SysForeignKeyColumn

data UnifyProblem
  = MissingTable UserTableName
  | MissingTableForRelationship UserUsing
  | MissingColumnForRelationship UserUsing
  | UnknownScalarType Text
  deriving (Show)

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
    Nothing -> refute (pure (MissingTable table))
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
    "int" -> pure IntType
    "nvarchar" -> pure NVarCharType
    t -> refute (pure (UnknownScalarType t))

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
            Left e -> error e
            Right as -> pure as
        Partial {} -> error "Incomplete output from SQL Server."
        Fail _ _ctx err -> error ("JSON parser error: " <> err)
