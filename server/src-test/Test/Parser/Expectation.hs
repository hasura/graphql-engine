-- | Build expectations for GraphQL field parsers. For now it focuses on updates
-- only.
--
-- See 'runUpdateFieldTest'.
module Test.Parser.Expectation
  ( UpdateTestSetup (..),
    UpdateExpectationBuilder (..),
    runUpdateFieldTest,
    module I,
  )
where

import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as HM
import Hasura.Backends.Postgres.SQL.Types (QualifiedTable)
import Hasura.Backends.Postgres.Types.Update (BackendUpdate (..), UpdateOpExpression (..))
import Hasura.GraphQL.Parser.Column (UnpreparedValue)
import Hasura.GraphQL.Parser.Internal.Parser (FieldParser (..))
import Hasura.GraphQL.Parser.Schema (Variable)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (AnnBoolExpFld (..), GBoolExp (..), OpExpG (..))
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Update (AnnotatedUpdateG (..))
import Hasura.RQL.Types.Column (ColumnInfo (..))
import Hasura.RQL.Types.Instances ()
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Vanilla))
import Language.GraphQL.Draft.Syntax qualified as Syntax
import Test.Hspec
import Test.Parser.Internal
import Test.Parser.Internal as I (ColumnInfoBuilder (..))
import Test.Parser.Monad

type PG = 'Postgres 'Vanilla

type BoolExp = GBoolExp PG (AnnBoolExpFld PG (UnpreparedValue PG))

type Output = MutationOutputG PG (RemoteRelationshipFieldWrapper UnpreparedValue) (UnpreparedValue PG)

type Field = Syntax.Field Syntax.NoFragments Variable

type Where = (ColumnInfoBuilder, [OpExpG PG (UnpreparedValue PG)])

type Update = (ColumnInfoBuilder, UpdateOpExpression (UnpreparedValue PG))

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
    utbOutput :: Output,
    -- | expected where condition(s), e.g. given a @nameColumn ::
    -- ColumnInfoBuilder@ and @oldValue :: UnpreparedValue PG@:
    --
    -- > [(nameColumn, [AEQ true oldvalue])]
    utbWhere :: [Where],
    -- | expected update clause(s), e.g. given a @nameColumn ::
    -- ColumnInfoBuilder@ and @newValue :: UnpreparedValue PG@:
    --
    -- > [(namecolumn, UpdateSet newValue)]
    utbUpdate :: [Update]
  }

-- | Run a test given the schema and field.
runUpdateFieldTest :: UpdateTestSetup -> Expectation
runUpdateFieldTest UpdateTestSetup {..} =
  case mkParser table utsColumns of
    SchemaTestT [] -> expectationFailure "expected at least one parser"
    SchemaTestT (FieldParser {fParser} : _xs) ->
      case fParser utsField of
        ParserTestT (Right annUpdate) -> do
          coerce annUpdate `shouldBe` expected
        ParserTestT (Left err) -> err
  where
    UpdateExpectationBuilder {..} = utsExpect

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
            aubUpdate = first mkColumnInfo <$> utbUpdate
          }

-- | Internal use only. The intended use is through 'runUpdateFieldTest'.
--
-- Build an 'AnnotatedUpdateG', to be used with 'mkAnnotatedUpdate'.
data AnnotatedUpdateBuilder = AnnotatedUpdateBuilder
  { -- | the main table for the update
    aubTable :: QualifiedTable,
    -- | the 'Output' clause, e.g., selection set, affected_rows, etc.
    aubOutput :: Output,
    -- | the table columns (all of them)
    aubColumns :: [ColumnInfo PG],
    -- | the where clause(s)
    aubWhere :: [(ColumnInfo PG, [OpExpG PG (UnpreparedValue PG)])],
    -- | the update statement(s)
    aubUpdate :: [(ColumnInfo PG, UpdateOpExpression (UnpreparedValue PG))]
  }

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
  AnnotatedUpdateBuilder ->
  AnnotatedUpdateG PG (RemoteRelationshipFieldWrapper UnpreparedValue) (UnpreparedValue PG)
mkAnnotatedUpdate AnnotatedUpdateBuilder {..} = AnnotatedUpdateG {..}
  where
    _auTable :: QualifiedTable
    _auTable = aubTable

    _auWhere :: (BoolExp, BoolExp)
    _auWhere =
      ( column [],
        BoolAnd $ fmap (\(c, ops) -> BoolFld $ AVColumn c ops) aubWhere
      )

    _auCheck :: BoolExp
    _auCheck = BoolAnd []

    _auBackend :: BackendUpdate (UnpreparedValue PG)
    _auBackend =
      BackendUpdate
        { updateOperations =
            HM.fromList $ fmap (bimap ciColumn id) aubUpdate
        }

    _auOutput :: Output
    _auOutput = aubOutput

    _auAllCols :: [ColumnInfo PG]
    _auAllCols = aubColumns

    column :: [OpExpG PG (UnpreparedValue PG)] -> BoolExp
    column stuff =
      BoolAnd
        . fmap (\c -> BoolFld . AVColumn c $ stuff)
        $ aubColumns
