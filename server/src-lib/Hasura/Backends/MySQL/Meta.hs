module Hasura.Backends.MySQL.Meta where


import           Control.Exception                     (throw)
import qualified Data.ByteString.Char8                 as B8
import           Data.FileEmbed                        (embedFile, makeRelativeToProject)
import qualified Data.HashMap.Strict                   as HM
import qualified Data.HashSet                          as HS
import qualified Data.Sequence.NonEmpty                as SNE
import           Data.String                           (fromString)
import           Database.MySQL.Base                   (Connection)
import           Database.MySQL.Base.Types             (Field (..))
import           Database.MySQL.Simple                 (Only (Only), query)
import           Database.MySQL.Simple.QueryResults    (QueryResults (..), convertError)
import           Database.MySQL.Simple.Result          (Result, ResultError (..), convert)
import           Hasura.Backends.MySQL.Instances.Types ()
import           Hasura.Backends.MySQL.Types
import           Hasura.Prelude
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend
import qualified Language.GraphQL.Draft.Syntax         as G


getMetadata :: ConnSourceConfig -> Connection -> IO (DBTablesMetadata 'MySQL)
getMetadata ConnSourceConfig{_cscDatabase} scConnection = do
  let sql = $(makeRelativeToProject "src-rsr/mysql_table_metadata.sql" >>= embedFile)
  results :: [InformationSchema] <- query scConnection (fromString . B8.unpack $ sql) (Only _cscDatabase)
  pure (mkMetadata results)


mkMetadata :: [InformationSchema] -> DBTablesMetadata 'MySQL
mkMetadata = foldr mergeMetadata HM.empty


mergeMetadata :: InformationSchema -> DBTablesMetadata 'MySQL -> DBTablesMetadata 'MySQL
mergeMetadata InformationSchema{..} =
  HM.insertWith
    mergeDBTableMetadata
    (TableName isTableName isTableSchema) $
    DBTableMetadata
      { _ptmiOid = OID 0
      , _ptmiColumns =
          [ RawColumnInfo
              { prciName = Column $ fromMaybe "" isColumnName
              , prciPosition = fromIntegral isOrdinalPosition
              , prciType = parseMySQLScalarType isColumnType -- TODO: This needs to become more precise by considering Field length and character-set
              , prciIsNullable = isIsNullable == "YES" -- ref: https://dev.mysql.com/doc/refman/8.0/en/information-schema-columns-table.html
              , prciDescription = Just $ G.Description isColumnComment
              }
          ]
      , _ptmiPrimaryKey = if isColumnKey == PRI
          then Just $
            PrimaryKey
              (Constraint
                (ConstraintName $ fromMaybe "" isConstraintName)
                (OID $ fromIntegral $ fromMaybe 0 isConstraintOrdinalPosition))
              (SNE.singleton (Column $ fromMaybe "" isColumnName))
          else Nothing
      , _ptmiUniqueConstraints = if isColumnKey == UNI
          then HS.singleton
                 (Constraint
                   (ConstraintName $ fromMaybe "" isConstraintName)
                   (OID $ fromIntegral $ fromMaybe 0 isConstraintOrdinalPosition))
          else HS.empty
      , _ptmiForeignKeys = if isColumnKey == MUL
          then HS.singleton
                 (ForeignKeyMetadata
                   (ForeignKey
                     (Constraint
                       (ConstraintName $ fromMaybe "" isConstraintName)
                       (OID $ fromIntegral $ fromMaybe 0 isConstraintOrdinalPosition))
                     (TableName
                       (fromMaybe "" isReferencedTableName)
                       (fromMaybe "" isReferencedTableSchema))
                     (HM.singleton
                       (Column $ fromMaybe "" isColumnName)
                       (Column $ fromMaybe "" isReferencedColumnName))
                   )
                 )
          else HS.empty
      , _ptmiViewInfo = Nothing
      , _ptmiDescription = Nothing
      , _ptmiExtraTableMetadata = ()
      }


mergeDBTableMetadata :: DBTableMetadata 'MySQL -> DBTableMetadata 'MySQL -> DBTableMetadata 'MySQL
mergeDBTableMetadata new existing =
  DBTableMetadata
    { _ptmiOid = OID 0
    , _ptmiColumns = _ptmiColumns existing <> _ptmiColumns new
    , _ptmiPrimaryKey = _ptmiPrimaryKey existing <|> _ptmiPrimaryKey new -- Only one column can be a PRIMARY KEY, so this is just a courtesy choice.
    , _ptmiUniqueConstraints = _ptmiUniqueConstraints existing <> _ptmiUniqueConstraints new -- union
    , _ptmiForeignKeys = _ptmiForeignKeys existing <> _ptmiForeignKeys new -- union
    , _ptmiViewInfo = _ptmiViewInfo existing <|> _ptmiViewInfo new
    , _ptmiDescription = _ptmiDescription existing <|> _ptmiDescription new
    , _ptmiExtraTableMetadata = ()
    }


data InformationSchema
  = InformationSchema
    { isTableSchema                :: !Text
    , isTableName                  :: !Text
    , isColumnName                 :: !(Maybe Text)
    , isOrdinalPosition            :: !Word
    , isColumnDefault              :: !(Maybe Text)
    , isIsNullable                 :: !Text
    , isDataType                   :: !(Maybe Text)
    , isColumnType                 :: !Text
    , isColumnKey                  :: !InformationSchemaColumnKey
    , isColumnComment              :: !Text
    , isConstraintName             :: !(Maybe Text)
    , isConstraintOrdinalPosition  :: !(Maybe Word)
    , isPositionInUniqueConstraint :: !(Maybe Word)
    , isReferencedTableSchema      :: !(Maybe Text)
    , isReferencedTableName        :: !(Maybe Text)
    , isReferencedColumnName       :: !(Maybe Text)
    } deriving (Show, Eq, Generic)
instance QueryResults InformationSchema where
  convertResults
    [ fisTableSchema
    , fisTableName
    , fisColumnName
    , fisOrdinalPosition
    , fisColumnDefault
    , fisIsNullable
    , fisDataType
    , fisColumnType
    , fisColumnKey
    , fisColumnComment
    , fisConstraintName
    , fisConstraintOrdinalPosition
    , fisPositionInUniqueConstraint
    , fisReferencedTableSchema
    , fisReferencedTableName
    , fisReferencedColumnName
    ]
    [ visTableSchema
    , visTableName
    , visColumnName
    , visOrdinalPosition
    , visColumnDefault
    , visIsNullable
    , visDataType
    , visColumnType
    , visColumnKey
    , visColumnComment
    , visConstraintName
    , visConstraintOrdinalPosition
    , visPositionInUniqueConstraint
    , visReferencedTableSchema
    , visReferencedTableName
    , visReferencedColumnName
    ]
    = InformationSchema
      (convert fisTableSchema                visTableSchema               )
      (convert fisTableName                  visTableName                 )
      (convert fisColumnName                 visColumnName                )
      (convert fisOrdinalPosition            visOrdinalPosition           )
      (convert fisColumnDefault              visColumnDefault             )
      (convert fisIsNullable                 visIsNullable                )
      (convert fisDataType                   visDataType                  )
      (convert fisColumnType                 visColumnType                )
      (convert fisColumnKey                  visColumnKey                 )
      (convert fisColumnComment              visColumnComment             )
      (convert fisConstraintName             visConstraintName            )
      (convert fisConstraintOrdinalPosition  visConstraintOrdinalPosition )
      (convert fisPositionInUniqueConstraint visPositionInUniqueConstraint)
      (convert fisReferencedTableSchema      visReferencedTableSchema     )
      (convert fisReferencedTableName        visReferencedTableName       )
      (convert fisReferencedColumnName       visReferencedColumnName      )
  convertResults fs vs = convertError fs vs 16
  -- ^ 'convertError' takes the number of expected columns for conversion as its third argument


data InformationSchemaColumnKey
  = PRI
  | UNI
  | MUL
  | BLANK -- ^ This field isn't NULLable and uses empty strings, by the looks of it.
    deriving (Show, Read, Eq, Generic)
instance Result InformationSchemaColumnKey where
  -- | ref: https://hackage.haskell.org/package/mysql-simple-0.4.5/docs/Database-MySQL-Simple-Result.html#v:convert
  -- specifies that the function is expected to throw a 'Database.MySQL.Simple.Result.ResultError'
  convert f mbs =
    case mbs of
      Nothing ->
        throw $
          UnexpectedNull
            (show $ fieldType f)
            "InformationSchemaColumnKey"
            (B8.unpack $ fieldName f)
            "COLUMN_KEY in INFORMATION_SCHEMA cannot be NULL"
      Just bs -> case bs of
        -- Could have used 'readMaybe' here, but we need the specific errors.
        "PRI" -> PRI -- ^ primay key
        "UNI" -> UNI -- ^ unique key
        "MUL" -> MUL -- ^ foreign key (`MUL`tiple allowed, non-unique key)
        "" -> BLANK
        x ->
          throw $
            ConversionFailed
              (show $ fieldType f)
              "InformationSchemaColumnKey"
              (B8.unpack $ fieldName f)
              ("COLUMN_KEY in INFORMATION_SCHEMA has value extraneous to the expected ENUM: " <> B8.unpack x)
