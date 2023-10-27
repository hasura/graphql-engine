-- | Build expectations for GraphQL field parsers. For now it focuses on updates
-- only.
--
-- See 'runUpdateFieldTest'.
module Test.Parser.Expectation
  ( UpdateTestSetup (..),
    UpdateExpectationBuilder (..),
    UpdateVariantBuilder (..),
    UpdateBatchBuilder (..),
    runUpdateFieldTest,
    module I,
    AnnotatedUpdateBuilder (..),
    mkAnnotatedUpdate,
    toBoolExp,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.Postgres.SQL.Types (QualifiedTable)
import Hasura.Backends.Postgres.Types.Update (PgUpdateVariant (..), UpdateOpExpression (..))
import Hasura.GraphQL.Parser.Internal.Parser (FieldParser (..))
import Hasura.GraphQL.Parser.Schema (Definition (..))
import Hasura.GraphQL.Parser.Variable (Variable (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (AnnBoolExpFld (..), AnnRedactionExp (..), GBoolExp (..), OpExpG (..))
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Update (AnnotatedUpdateG (..))
import Hasura.RQL.IR.Update.Batch (UpdateBatch (..))
import Hasura.RQL.IR.Value (UnpreparedValue)
import Hasura.RQL.Types.BackendType (BackendSourceKind (PostgresVanillaKind), BackendType (Postgres), PostgresKind (Vanilla))
import Hasura.RQL.Types.Column (ColumnInfo (..))
import Hasura.RQL.Types.Common (ResolvedWebhook, SourceName (..))
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Source (DBObjectsIntrospection (..), SourceInfo (..))
import Hasura.RQL.Types.SourceCustomization (ResolvedSourceCustomization (..))
import Hasura.Table.Cache (TableInfo (..))
import Language.GraphQL.Draft.Syntax qualified as Syntax
import Test.Hspec
import Test.Parser.Internal
import Test.Parser.Internal as I (ColumnInfoBuilder (..), mkColumnInfo, mkTable)
import Test.Parser.Monad

type PG = 'Postgres 'Vanilla

type BoolExp = GBoolExp PG (AnnBoolExpFld PG (UnpreparedValue PG))

type Output r = MutationOutputG PG r (UnpreparedValue PG)

type Field = Syntax.Field Syntax.NoFragments Variable

type Update = UpdateVariantBuilder ColumnInfoBuilder

-- | Holds all the information required to setup and run a field parser update
-- test.
data UpdateTestSetup = UpdateTestSetup
  { -- | name of the table
    utsTable :: Text,
    -- | table columns
    utsColumns :: [ColumnInfoBuilder],
    -- | expectation
    utsExpect :: UpdateExpectationBuilder,
    -- | GrqphQL field, see Test.Parser.Parser
    utsField :: Field
  }

-- | Build the expected output columns, where and update clauses.
data UpdateExpectationBuilder = UpdateExpectationBuilder
  { -- | build the expected selection set/output, e.g.
    --
    -- > MOutMultirowFields [("affected_rows", MCount)]
    utbOutput :: Output (RemoteRelationshipFieldWrapper UnpreparedValue),
    -- | expected update clause(s), including the where condition as update operations,
    -- e.g. given a @nameColumn :: ColumnInfoBuilder@ and
    -- @newValue :: UnpreparedValue PG@:
    --
    -- > SingleBatchUpdate (UpdateBatchBuilder [(nameColumn, [AEQ true oldvalue])] [(nameColumn, UpdateSet newValue)])
    utbUpdate :: Update
  }

-- | Run a test given the schema and field.
runUpdateFieldTest :: UpdateTestSetup -> Expectation
runUpdateFieldTest UpdateTestSetup {..} =
  case runSchemaTest sourceInfo $ mkParser ((tableInfoBuilder table) {columns = utsColumns}) of
    [] -> expectationFailure "expected at least one parser"
    parsers ->
      case find (byName (Syntax._fName utsField)) parsers of
        Nothing -> expectationFailure $ "could not find parser " <> show (Syntax._fName utsField)
        Just FieldParser {..} -> do
          annUpdate <- runParserTest $ fParser utsField
          coerce annUpdate `shouldBe` expected
  where
    UpdateExpectationBuilder {..} = utsExpect

    sourceInfo :: SourceInfo ('Postgres 'Vanilla)
    sourceInfo =
      SourceInfo
        { _siName = SNDefault,
          _siSourceKind = PostgresVanillaKind,
          _siTables = HashMap.singleton table tableInfo,
          _siFunctions = mempty,
          _siNativeQueries = mempty,
          _siStoredProcedures = mempty,
          _siLogicalModels = mempty,
          _siConfiguration = notImplementedYet "SourceConfig",
          _siQueryTagsConfig = Nothing,
          _siCustomization = ResolvedSourceCustomization mempty mempty HasuraCase Nothing,
          _siDbObjectsIntrospection = DBObjectsIntrospection mempty mempty mempty mempty
        }

    byName :: Syntax.Name -> Parser -> Bool
    byName name FieldParser {..} = name == dName fDefinition

    table :: QualifiedTable
    table = mkTable utsTable

    tableInfo :: TableInfo PG
    tableInfo =
      buildTableInfo
        TableInfoBuilder
          { table = table,
            columns = utsColumns,
            relations = []
          }

    expected :: AnnotatedUpdateG PG (RemoteRelationshipFieldWrapper UnpreparedValue) (UnpreparedValue PG)
    expected =
      mkAnnotatedUpdate
        AnnotatedUpdateBuilder
          { aubTable = table,
            aubOutput = utbOutput,
            aubColumns = mkColumnInfo <$> utsColumns,
            aubUpdateVariant = mkUpdateColumns utbUpdate
          }
    mkUpdateColumns :: UpdateVariantBuilder ColumnInfoBuilder -> UpdateVariantBuilder (ColumnInfo PG)
    mkUpdateColumns = fmap mkColumnInfo

-- | Internal use only. The intended use is through 'runUpdateFieldTest'.
--
-- Build an 'AnnotatedUpdateG', to be used with 'mkAnnotatedUpdate'.
data AnnotatedUpdateBuilder r = AnnotatedUpdateBuilder
  { -- | the main table for the update
    aubTable :: QualifiedTable,
    -- | the 'Output' clause, e.g., selection set, affected_rows, etc.
    aubOutput :: Output r,
    -- | the table columns (all of them)
    aubColumns :: [ColumnInfo PG],
    -- | the update statement(s)
    aubUpdateVariant :: UpdateVariantBuilder (ColumnInfo PG)
  }

data UpdateVariantBuilder col
  = SingleBatchUpdate (UpdateBatchBuilder col)
  | MultipleBatchesUpdate [UpdateBatchBuilder col]
  deriving stock (Functor)

data UpdateBatchBuilder col = UpdateBatchBuilder
  { ubbWhere :: [(col, [OpExpG PG (UnpreparedValue PG)])],
    ubbOperations :: [(col, UpdateOpExpression (UnpreparedValue PG))]
  }
  deriving stock (Functor)

-- | 'RemoteRelationshipField' cannot have Eq/Show instances, so we're wrapping
-- it.
newtype RemoteRelationshipFieldWrapper vf = RemoteRelationshipFieldWrapper (RemoteRelationshipField vf)

instance Show (RemoteRelationshipFieldWrapper vf) where
  show =
    error "Test.Parser.Expectation: no Show implementation for RemoteRelationshipFieldWrapper"

instance Eq (RemoteRelationshipFieldWrapper vf) where
  (==) =
    error "Test.Parser.Expectation: no Eq implementation for RemoteRelationshipFieldWrapper"

-- | Internal use, see 'runUpdateFieldTest'.
mkAnnotatedUpdate ::
  forall r.
  AnnotatedUpdateBuilder r ->
  AnnotatedUpdateG PG r (UnpreparedValue PG)
mkAnnotatedUpdate AnnotatedUpdateBuilder {..} = AnnotatedUpdateG {..}
  where
    _auTable :: QualifiedTable
    _auTable = aubTable

    _auCheck :: BoolExp
    _auCheck = BoolAnd []

    _auUpdateVariant :: PgUpdateVariant 'Vanilla (UnpreparedValue PG)
    _auUpdateVariant =
      case aubUpdateVariant of
        SingleBatchUpdate batch ->
          SingleBatch $ mapUpdateBatch batch
        MultipleBatchesUpdate batches ->
          MultipleBatches $ mapUpdateBatch <$> batches

    mapUpdateBatch :: UpdateBatchBuilder (ColumnInfo PG) -> UpdateBatch ('Postgres 'Vanilla) UpdateOpExpression (UnpreparedValue PG)
    mapUpdateBatch UpdateBatchBuilder {..} =
      UpdateBatch
        { _ubWhere = toBoolExp ubbWhere,
          _ubOperations = HashMap.fromList $ fmap (first ciColumn) ubbOperations
        }

    _auOutput :: Output r
    _auOutput = aubOutput

    _auAllCols :: [ColumnInfo PG]
    _auAllCols = aubColumns

    _auUpdatePermissions :: BoolExp
    _auUpdatePermissions =
      BoolAnd
        . fmap (\c -> BoolField . AVColumn c NoRedaction $ [])
        $ aubColumns

    _auNamingConvention :: Maybe NamingCase
    _auNamingConvention = Just HasuraCase

    _auValidateInput :: Maybe (ValidateInput ResolvedWebhook)
    _auValidateInput = Nothing

toBoolExp :: [(ColumnInfo PG, [OpExpG PG (UnpreparedValue PG)])] -> BoolExp
toBoolExp = BoolAnd . fmap (\(c, ops) -> BoolField $ AVColumn c NoRedaction ops)
