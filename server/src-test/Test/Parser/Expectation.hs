-- | Build expectations for GraphQL field parsers. For now it focuses on updates
-- only.
--
-- See 'runUpdateFieldTest'.
module Test.Parser.Expectation
  ( UpdateTestSetup (..),
    UpdateExpectationBuilder (..),
    BackendUpdateBuilder (..),
    MultiRowUpdateBuilder (..),
    runUpdateFieldTest,
    module I,
    AnnotatedUpdateBuilder (..),
    mkAnnotatedUpdate,
    toBoolExp,
  )
where

import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as HM
import Hasura.Backends.Postgres.SQL.Types (QualifiedTable)
import Hasura.Backends.Postgres.Types.Update (BackendUpdate (..), MultiRowUpdate (..), UpdateOpExpression (..))
import Hasura.GraphQL.Parser.Internal.Parser (FieldParser (..))
import Hasura.GraphQL.Parser.Schema (Definition (..))
import Hasura.GraphQL.Parser.Variable (Variable (..))
import Hasura.GraphQL.Schema.NamingCase
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (AnnBoolExpFld (..), GBoolExp (..), OpExpG (..))
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Update (AnnotatedUpdateG (..))
import Hasura.RQL.IR.Value (UnpreparedValue)
import Hasura.RQL.Types.Column (ColumnInfo (..))
import Hasura.RQL.Types.Instances ()
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Vanilla))
import Language.GraphQL.Draft.Syntax qualified as Syntax
import Test.Hspec
import Test.Parser.Internal
import Test.Parser.Internal as I (ColumnInfoBuilder (..), mkColumnInfo, mkTable)
import Test.Parser.Monad

type PG = 'Postgres 'Vanilla

type BoolExp = GBoolExp PG (AnnBoolExpFld PG (UnpreparedValue PG))

type Output r = MutationOutputG PG r (UnpreparedValue PG)

type Field = Syntax.Field Syntax.NoFragments Variable

type Where = (ColumnInfoBuilder, [OpExpG PG (UnpreparedValue PG)])

type Update = BackendUpdateBuilder ColumnInfoBuilder

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
    -- | expected where condition(s), e.g. given a @nameColumn ::
    -- ColumnInfoBuilder@ and @oldValue :: UnpreparedValue PG@:
    --
    -- > [(nameColumn, [AEQ true oldvalue])]
    utbWhere :: [Where],
    -- | expected update clause(s), e.g. given a @nameColumn ::
    -- ColumnInfoBuilder@ and @newValue :: UnpreparedValue PG@:
    --
    -- > [(namecolumn, UpdateSet newValue)]
    utbUpdate :: Update
  }

-- | Run a test given the schema and field.
runUpdateFieldTest :: UpdateTestSetup -> Expectation
runUpdateFieldTest UpdateTestSetup {..} =
  case mkParser (TableInfoBuilder table utsColumns) of
    SchemaTestM [] -> expectationFailure "expected at least one parser"
    SchemaTestM parsers ->
      case find (byName (Syntax._fName utsField)) parsers of
        Nothing -> expectationFailure $ "could not find parser " <> show (Syntax._fName utsField)
        Just FieldParser {..} -> do
          annUpdate <- runParserTestM $ fParser utsField
          coerce annUpdate `shouldBe` expected
  where
    UpdateExpectationBuilder {..} = utsExpect

    byName :: Syntax.Name -> Parser -> Bool
    byName name FieldParser {..} = name == dName fDefinition

    table :: QualifiedTable
    table = mkTable utsTable

    expected :: AnnotatedUpdateG PG (RemoteRelationshipFieldWrapper UnpreparedValue) (UnpreparedValue PG)
    expected =
      mkAnnotatedUpdate
        AnnotatedUpdateBuilder
          { aubTable = table,
            aubOutput = utbOutput,
            aubColumns = mkColumnInfo <$> utsColumns,
            aubWhere = first mkColumnInfo <$> utbWhere,
            aubUpdate = mkUpdateColumns utbUpdate
          }
    mkUpdateColumns :: BackendUpdateBuilder ColumnInfoBuilder -> BackendUpdateBuilder (ColumnInfo PG)
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
    -- | the where clause(s)
    aubWhere :: [(ColumnInfo PG, [OpExpG PG (UnpreparedValue PG)])],
    -- | the update statement(s)
    aubUpdate :: BackendUpdateBuilder (ColumnInfo PG)
  }

data BackendUpdateBuilder col
  = UpdateTable [(col, UpdateOpExpression (UnpreparedValue PG))]
  | UpdateMany [MultiRowUpdateBuilder col]
  deriving stock (Functor)

data MultiRowUpdateBuilder col = MultiRowUpdateBuilder
  { mrubWhere :: [(col, [OpExpG PG (UnpreparedValue PG)])],
    mrubUpdate :: [(col, UpdateOpExpression (UnpreparedValue PG))]
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

    _auWhere :: (BoolExp, BoolExp)
    _auWhere = (column, toBoolExp aubWhere)

    _auCheck :: BoolExp
    _auCheck = BoolAnd []

    _auBackend :: BackendUpdate 'Vanilla (UnpreparedValue PG)
    _auBackend =
      case aubUpdate of
        UpdateTable items ->
          BackendUpdate $
            HM.fromList $
              fmap (first ciColumn) items
        UpdateMany rows ->
          BackendMultiRowUpdate $ fmap mapRows rows

    mapRows :: MultiRowUpdateBuilder (ColumnInfo PG) -> MultiRowUpdate 'Vanilla (UnpreparedValue PG)
    mapRows MultiRowUpdateBuilder {..} =
      MultiRowUpdate
        { mruWhere = toBoolExp mrubWhere,
          mruExpression = HM.fromList $ fmap (bimap ciColumn id) mrubUpdate
        }

    _auOutput :: Output r
    _auOutput = aubOutput

    _auAllCols :: [ColumnInfo PG]
    _auAllCols = aubColumns

    column :: BoolExp
    column =
      BoolAnd
        . fmap (\c -> BoolField . AVColumn c $ [])
        $ aubColumns

    _auNamingConvention :: Maybe NamingCase
    _auNamingConvention = Just HasuraCase

toBoolExp :: [(ColumnInfo PG, [OpExpG PG (UnpreparedValue PG)])] -> BoolExp
toBoolExp = BoolAnd . fmap (\(c, ops) -> BoolField $ AVColumn c ops)
