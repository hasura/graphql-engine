module Hasura.Sources.MySQL.Metadata
  ( fetchTables
  ) where

import Hasura.Prelude
import Data.Sequence.NonEmpty
import Data.Sequence (fromList)

import qualified Database.MySQL.Base as My
import Control.Concurrent.MVar
import qualified System.IO.Streams.List as IO (toList)
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as Map
import qualified Language.GraphQL.Draft.Syntax as G

import Hasura.RQL.Types.Table
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Column
import Hasura.SQL.Types
import Hasura.Sources

import Debug.Trace

fetchTables :: IO TableCache
fetchTables = do
  conn <- fromJust <$> readMVar mySQLConnection
  {-
  = TableCoreInfo
  { _tciName              :: !QualifiedTable
  , _tciDescription       :: !(Maybe PGDescription)
  , _tciSystemDefined     :: !SystemDefined
  , _tciFieldInfoMap      :: !(FieldInfoMap field)
  , _tciPrimaryKey        :: !(Maybe (PrimaryKey primaryKeyColumn))
  , _tciUniqueConstraints :: !(HashSet Constraint)
  -- ^ Does /not/ include the primary key; use 'tciUniqueOrPrimaryKeyConstraints' if you need both.
  , _tciForeignKeys       :: !(HashSet ForeignKey)
  , _tciViewInfo          :: !(Maybe ViewInfo)
  , _tciEnumValues        :: !(Maybe EnumValues)
  , _tciCustomConfig      :: !TableConfig
  -}
  tables <- fmap (\case [My.MySQLText schema, My.MySQLText name, My.MySQLText typ] -> (schema, name, typ)) <$>
    ( IO.toList =<< snd <$> My.query_ conn
      ( My.Query
        "SELECT TABLE_SCHEMA, TABLE_NAME, TABLE_TYPE \
        \FROM INFORMATION_SCHEMA.TABLES \
        \WHERE TABLE_TYPE='BASE TABLE'"
      )
    )
  {-
  = PGColumnInfo
  { pgiColumn      :: !PGCol
  , pgiName        :: !G.Name
  -- ^ field name exposed in GraphQL interface
  , pgiPosition    :: !Int
  , pgiType        :: !PGColumnType
  , pgiIsNullable  :: !Bool
  , pgiDescription :: !(Maybe G.Description)
  -}
  columns <- fmap (
    \case [   My.MySQLText tableSchema
            , My.MySQLText tableName
            , My.MySQLText columnName
            , My.MySQLInt32U ordinalPosition
            , My.MySQLText isNullable
            , My.MySQLText dataType
            , My.MySQLText columnKey
            ] -> (tableSchema, tableName, PGColumnInfo
                (unsafePGCol columnName)
                (G.unsafeMkName columnName)
                (fromIntegral ordinalPosition)
                (PGColumnScalar (textToPGScalarType dataType))
                (isNullable == "YES")
                Nothing, columnKey)) <$>
    ( IO.toList =<< snd <$> My.query_ conn
      ( My.Query
        "SELECT TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME, ORDINAL_POSITION, IS_NULLABLE, DATA_TYPE, COLUMN_KEY \
        \FROM INFORMATION_SCHEMA.COLUMNS"
      )
    )
  foreigns <- fmap (
    \case [   My.MySQLText tableSchema
            , My.MySQLText tableName
            , columnName -- TODO maybe text
            , constraintName -- TODO maybe text
            , referencedTableSchema -- TODO maybe text
            , refenencedTableName -- TODO maybe text
            , referencedColumnName -- TODO maybe text
            ] -> (tableSchema, tableName, columnName, constraintName, referencedTableSchema, refenencedTableName, referencedColumnName)) <$>
    ( IO.toList =<< snd <$> My.query_ conn
      ( My.Query
        "SELECT TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME, CONSTRAINT_NAME, REFERENCED_TABLE_SCHEMA, REFERENCED_TABLE_NAME, REFERENCED_COLUMN_NAME \
        \FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE"
      )
    )

  tableCache' <- for tables \table -> do
    let (tableSchema, tableName, tableType) = table
        tableIden = QualifiedObject (SchemaName tableSchema) (TableName tableName)
    if tableType == "BASE TABLE"
      then do
      fieldInfoMap <- Map.fromList . catMaybes <$> for columns \column -> do
        let (   tableSchema'
              , tableName'
              , pgci
              , columnKey
              ) = column
        if tableSchema == tableSchema' && tableName == tableName'
          then Just <$> do
          return (FieldName (G.unName (pgiName pgci)), FIColumn pgci)
          else return Nothing
      let primaryKeyCols = flip mapMaybe foreigns
            \case (tableSchema', tableName', columnName, constraintName, referencedTableSchema, refenencedTableName, referencedColumnName) ->
                    if tableSchema == tableSchema' && tableName == tableName' && constraintName == My.MySQLText "PRIMARY"
                    then
                      case columnName of
                        My.MySQLText t -> Just t
                        _ -> Nothing
                    else Nothing
          primKey = case fmap (\case (_,_,pgci,_) -> pgci) $ mapMaybe (\col -> find (\(ts', tn', pgci, _) -> tableSchema == ts' && tableName == tn' && G.unName (pgiName pgci) == col) columns) primaryKeyCols of
            [] -> Nothing
            -- TODO we're inserting some fake data here - MySQL has no OID, I think?
            (c:cs) -> Just $ PrimaryKey (Constraint (ConstraintName "primarykey") (OID 0)) (c :<|| fromList cs)

      let tci = TableCoreInfo
                tableIden
                Nothing
                (SystemDefined False)
                fieldInfoMap
                primKey -- PrimaryKey
                mempty -- _uniqueConstraints
                mempty -- _foreignKeys
                Nothing -- ViewInfo
                Nothing -- EnumValues
                emptyTableConfig
                MySQLDB
      let ti = TableInfo tci mempty mempty
      return $ Just (tableIden , ti)
      else return Nothing



  return $ Map.fromList $ catMaybes tableCache'
