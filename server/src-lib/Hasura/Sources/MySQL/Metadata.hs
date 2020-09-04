module Hasura.Sources.MySQL.Metadata
  ( fetchTables
  ) where

import Hasura.Prelude
import Data.Sequence.NonEmpty
import Data.Sequence (fromList)

import qualified Database.MySQL.Simple as My
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
  tables :: [(Text, Text, Text)] <-
    ( My.query_ conn
      (
        "SELECT TABLE_SCHEMA, TABLE_NAME, TABLE_TYPE \
        \FROM INFORMATION_SCHEMA.TABLES \
        \WHERE TABLE_TYPE='BASE TABLE' AND TABLE_SCHEMA NOT IN ('mysql', 'sys', 'performance_schema')"
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
    \case (   tableSchema :: Text
            , tableName :: Text
            , columnName :: Text
            , ordinalPosition
            , (isNullable :: Text)
            , dataType :: Text -- TODO White lie: actually this field is Nullable. But I think we're not interested in those cases?
            , columnKey :: Text
            ) -> (tableSchema, tableName, PGColumnInfo
                (unsafePGCol columnName)
                (G.unsafeMkName columnName)
                ordinalPosition
                (PGColumnScalar (textToPGScalarType dataType))
                (isNullable == "YES")
                Nothing, columnKey)) <$>
    ( My.query_ conn
      (
        "SELECT TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME, ORDINAL_POSITION, IS_NULLABLE, DATA_TYPE, COLUMN_KEY \
        \FROM INFORMATION_SCHEMA.COLUMNS"
      )
    )
  foreigns <- fmap (
    \case (   tableSchema
            , tableName
            , columnName
            , constraintName
            , referencedTableSchema
            , refenencedTableName
            , referencedColumnName
            ) -> (ForeignRow
                  tableSchema
                  tableName
                  columnName
                  constraintName
                  referencedTableSchema
                  refenencedTableName
                  referencedColumnName)) <$>
    ( My.query_ conn
      (
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
      let primaryKeyCols = flip mapMaybe foreigns \forRow ->
            if    tableSchema == frTableSchema forRow
               && tableName == frTableName forRow
               && frConstraintName forRow == Just "PRIMARY"
            then frColumnName forRow
            else Nothing
          primKey = case fmap (\case (_,_,pgci,_) -> pgci) $ mapMaybe (\col -> find (\(ts', tn', pgci, _) -> tableSchema == ts' && tableName == tn' && G.unName (pgiName pgci) == col) columns) primaryKeyCols of
            [] -> Nothing
            -- TODO we're inserting some fake data here - MySQL has no OID, I think?
            (c:cs) -> Just $ PrimaryKey (Constraint (ConstraintName "primarykey") (OID 0)) (c :<|| fromList cs)

      let tci = TableCoreInfo
                tableIden
                Nothing
                (SystemDefined False)
                (  fieldInfoMap
                <> (FIRelationship <$> generateRelationshipsFor tableSchema tableName ObjRel foreigns)
                <> (FIRelationship <$> generateRelationshipsFor tableSchema tableName ArrRel foreigns))
                primKey
                mempty -- _uniqueConstraints
                mempty -- _foreignKey
                Nothing -- ViewInfo
                Nothing -- EnumValues
                emptyTableConfig
                MySQLDB
      let ti = TableInfo tci mempty mempty
      return $ Just (tableIden , ti)
      else return Nothing

  return $ Map.fromList $ catMaybes tableCache'

data ForeignRow = ForeignRow
  { frTableSchema :: Text
  , frTableName :: Text
  , frColumnName :: Maybe Text
  , frConstraintName :: Maybe Text
  , frRefTableSchema :: Maybe Text
  , frRefTableName :: Maybe Text
  , frRefColumnName :: Maybe Text
  }

{-
  = RelInfo
  { riName       :: !RelName
  , riType       :: !RelType
  , riMapping    :: !(HashMap PGCol PGCol)
  , riRTable     :: !QualifiedTable
  , riIsManual   :: !Bool
  , riIsNullable :: !Bool
-}
generateRelationshipsFor :: Text -> Text -> RelType -> [ForeignRow] -> FieldInfoMap RelInfo
generateRelationshipsFor tableSchema tableName relType foreigns =
  let
    sideFilter =
      \case ForeignRow{..} ->
              case relType of
                ObjRel -> frTableSchema == tableSchema && frTableName == tableName
                ArrRel -> frRefTableSchema == Just tableSchema && frRefTableName == Just tableName
    relevant = filter sideFilter foreigns
    convert :: ForeignRow -> Maybe RelInfo
    convert ForeignRow{..} = do
      let fromSchema = frTableSchema
          fromTable  = frTableName
      fromColumn <- frColumnName
      toSchema <- frRefTableSchema
      toTable  <- frRefTableName
      toColumn <- frRefColumnName
      let riName = RelName . mkNonEmptyTextUnsafe $
            case relType of
              ObjRel -> toTable <> "_object"
              ArrRel -> fromTable <> "_array"
          riType = relType
          riMapping =
            case relType of
              ObjRel -> Map.singleton (unsafePGCol fromColumn) (unsafePGCol toColumn)
              ArrRel -> Map.singleton (unsafePGCol toColumn) (unsafePGCol fromColumn)
          riRTable =
            case relType of
              ObjRel -> QualifiedObject (SchemaName toSchema) (TableName toTable)
              ArrRel -> QualifiedObject (SchemaName fromSchema) (TableName fromTable)
          riIsManual = False
          riIsNullable = True -- TODO figure out right value?
      return RelInfo{..}
  in Map.fromList $ mapMaybe (convert >=> \x -> Just (fromRel $ riName x , x)) relevant
