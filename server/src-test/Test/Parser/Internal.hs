-- | Internal helper module. Some things re-exported by
-- 'Test.Parser.Expectation'.
module Test.Parser.Internal
  ( mkTable,
    ColumnInfoBuilder (..),
    mkColumnInfo,
    mkParser,
  )
where

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Text.Casing qualified as C
import Hasura.Backends.Postgres.Instances.Schema (updateOperators)
import Hasura.Backends.Postgres.SQL.Types (QualifiedObject (..), QualifiedTable, TableName (..), unsafePGCol)
import Hasura.Backends.Postgres.Types.Update (BackendUpdate (..))
import Hasura.GraphQL.Parser.Internal.Input (InputFieldsParser)
import Hasura.GraphQL.Parser.Internal.Parser (FieldParser (..))
import Hasura.GraphQL.Schema.Build qualified as Build
import Hasura.GraphQL.Schema.Common (Scenario (Frontend))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (AnnBoolExpFld (..), GBoolExp (..), PartialSQLExp (..))
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Update (AnnotatedUpdateG (..))
import Hasura.RQL.IR.Value (UnpreparedValue (..))
import Hasura.RQL.Types.Column (ColumnInfo (..), ColumnMutability (..), ColumnType (..))
import Hasura.RQL.Types.Common (Comment (..), FieldName (..), SystemDefined (..))
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.Permission (AllowedRootFields (..))
import Hasura.RQL.Types.Source (SourceInfo)
import Hasura.RQL.Types.Table (CustomRootField (..), FieldInfo (..), RolePermInfo (..), SelPermInfo (..), TableConfig (..), TableCoreInfoG (..), TableCustomRootFields (..), TableInfo (..), UpdPermInfo (..))
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Vanilla))
import Language.GraphQL.Draft.Syntax (unsafeMkName)
import Test.Parser.Monad

type PG = 'Postgres 'Vanilla

type Parser = FieldParser ParserTestT (AnnotatedUpdateG PG (RemoteRelationshipField UnpreparedValue) (UnpreparedValue PG))

-- | Create a table by its name, using the public schema.
mkTable :: Text -> QualifiedTable
mkTable name =
  QualifiedObject
    { qSchema = "public",
      qName = TableName name
    }

-- | Build a column, see 'mkColumnInfo'.
data ColumnInfoBuilder = ColumnInfoBuilder
  { -- | name of the column
    cibName :: Text,
    -- | Column type, e.g.
    --
    -- > ColumnScalar PGText
    cibType :: ColumnType PG,
    -- | whether the column is nullable or not
    cibNullable :: Bool
  }

-- | Create a column using the provided 'ColumnInfoBuilder' and defaults.
--
-- Note that all permissions are enabled by default.
mkColumnInfo :: ColumnInfoBuilder -> ColumnInfo PG
mkColumnInfo ColumnInfoBuilder {..} =
  ColumnInfo
    { ciColumn = unsafePGCol cibName,
      ciName = unsafeMkName cibName,
      ciPosition = 0,
      ciType = cibType,
      ciIsNullable = cibNullable,
      ciDescription = Nothing,
      ciMutability = columnMutability
    }
  where
    columnMutability :: ColumnMutability
    columnMutability =
      ColumnMutability
        { _cmIsInsertable = True,
          _cmIsUpdatable = True
        }

-- | Create a parser for the provided table and columns.
--
-- No special permissions, required headers, filters, etc., are set.
--
-- This will not work for inserts and deletes (see @rolePermInfo@ below).
mkParser :: QualifiedTable -> [ColumnInfoBuilder] -> SchemaTestT [Parser]
mkParser table cib =
  Build.buildTableUpdateMutationFields
    backendUpdateParser
    Frontend
    sourceInfo
    table
    tableInfo
    name
  where
    backendUpdateParser ::
      TableInfo PG ->
      SchemaTestT (InputFieldsParser ParserTestT (BackendUpdate (UnpreparedValue PG)))
    backendUpdateParser ti =
      fmap BackendUpdate <$> updateOperators ti updPermInfo

    updPermInfo :: UpdPermInfo PG
    updPermInfo =
      UpdPermInfo
        { upiCols = HS.fromList . fmap (unsafePGCol . cibName) $ cib,
          upiTable = table,
          upiFilter = upiFilter,
          upiCheck = Nothing,
          upiSet = mempty,
          upiBackendOnly = False,
          upiRequiredHeaders = mempty
        }

    columnInfos :: [ColumnInfo PG]
    columnInfos = mkColumnInfo <$> cib

    upiFilter :: GBoolExp PG (AnnBoolExpFld PG (PartialSQLExp PG))
    upiFilter = BoolAnd $ fmap (\ci -> BoolFld $ AVColumn ci []) columnInfos

    ------------------------------------------
    sourceInfo :: SourceInfo PG
    sourceInfo = undefined

    ------------------------------------------
    tableInfo :: TableInfo PG
    tableInfo =
      TableInfo
        { _tiCoreInfo = tableCoreInfo,
          _tiRolePermInfoMap = mempty,
          _tiEventTriggerInfoMap = mempty,
          _tiAdminRolePermInfo = rolePermInfo
        }

    tableCoreInfo :: TableCoreInfoG PG (FieldInfo PG) (ColumnInfo PG)
    tableCoreInfo =
      TableCoreInfo
        { _tciName = table,
          _tciDescription = Nothing,
          _tciSystemDefined = SystemDefined False,
          _tciFieldInfoMap = fieldInfoMap,
          _tciPrimaryKey = Nothing,
          _tciUniqueConstraints = mempty,
          _tciForeignKeys = mempty,
          _tciViewInfo = Nothing,
          _tciEnumValues = Nothing,
          _tciCustomConfig = tableConfig,
          _tciExtraTableMetadata = ()
        }

    rolePermInfo :: RolePermInfo PG
    rolePermInfo =
      RolePermInfo
        { _permIns = Nothing,
          _permSel = Just selPermInfo,
          _permUpd = Just updPermInfo,
          _permDel = Nothing
        }

    fieldInfoMap :: HM.HashMap FieldName (FieldInfo PG)
    fieldInfoMap =
      HM.fromList
        . fmap toHashPair
        $ cib

    tableConfig :: TableConfig PG
    tableConfig =
      TableConfig
        { _tcCustomRootFields = tableCustomRootFields,
          _tcColumnConfig = mempty,
          _tcCustomName = Nothing,
          _tcComment = Automatic
        }

    selPermInfo :: SelPermInfo PG
    selPermInfo =
      SelPermInfo
        { spiCols = HM.fromList . fmap ((,Nothing) . unsafePGCol . cibName) $ cib,
          spiComputedFields = mempty,
          spiFilter = upiFilter,
          spiLimit = Nothing,
          spiAllowAgg = True,
          spiRequiredHeaders = mempty,
          spiAllowedQueryRootFields = ARFAllowAllRootFields,
          spiAllowedSubscriptionRootFields = ARFAllowAllRootFields
        }

    tableCustomRootFields :: TableCustomRootFields
    tableCustomRootFields =
      TableCustomRootFields
        { _tcrfSelect = customRootField,
          _tcrfSelectByPk = customRootField,
          _tcrfSelectAggregate = customRootField,
          _tcrfInsert = customRootField,
          _tcrfInsertOne = customRootField,
          _tcrfUpdate = customRootField,
          _tcrfUpdateByPk = customRootField,
          _tcrfDelete = customRootField,
          _tcrfDeleteByPk = customRootField
        }

    customRootField :: CustomRootField
    customRootField =
      CustomRootField
        { _crfName = Nothing,
          _crfComment = Automatic
        }
    ------------------------------------------
    name :: C.GQLNameIdentifier
    name = C.fromName $ unsafeMkName "test"

toHashPair :: ColumnInfoBuilder -> (FieldName, FieldInfo PG)
toHashPair cib = (coerce $ cibName cib, FIColumn $ mkColumnInfo cib)
