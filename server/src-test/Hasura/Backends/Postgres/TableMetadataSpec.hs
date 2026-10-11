-- | Tests for Postgres-family table/function metadata introspection after
-- deprecating the import of database @COMMENT@s into the GraphQL schema.
--
-- 1. The table introspection queries (PostgreSQL, Citus, CockroachDB) and the
--    function metadata query must not execute a read of the comment catalogs
--    (@pg_description@ / @col_description@ / @obj_description@). Assertions run
--    against the executable SQL, after stripping SQL comments, so that
--    explanatory comments in the files cannot mask a real result.
--
-- 2. 'pgStripImportedDescriptions' (the 'normaliseDBObjectsIntrospection' hook for
--    Postgres) unconditionally drops table, column and function descriptions,
--    leaving scalars/logical models untouched. This guards the stored
--    introspection fallback.
module Hasura.Backends.Postgres.TableMetadataSpec (spec) where

import Data.Aeson qualified as J
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Database.PG.Query qualified as PGQ
import Hasura.Backends.Postgres.DDL.Source (pgFunctionMetadataQuery, pgStripImportedDescriptions, tableMetadata)
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.Function.Cache (FunctionOverloads (..))
import Hasura.Prelude
import Hasura.RQL.Types.BackendType (BackendType (..), PostgresKind (..))
import Hasura.RQL.Types.Column (ColumnMutability (..), RawColumnInfo (..), RawColumnType (..))
import Hasura.RQL.Types.Common (OID (..))
import Hasura.RQL.Types.Source (DBObjectsIntrospection (..))
import Hasura.RQL.Types.Source.TableType qualified as STT
import Hasura.Table.Cache (DBTableMetadata (..))
import Language.GraphQL.Draft.Syntax qualified as G
import Test.Hspec

--------------------------------------------------------------------------------
-- 1. Introspection SQL shape

-- | Remove SQL line comments (@-- ...@ to end of line) so assertions run against
-- the executable SQL only. The introspection files contain no block comments.
stripSqlComments :: Text -> Text
stripSqlComments = T.unlines . map (fst . T.breakOn "--") . T.lines

executableMentions :: Text -> PGQ.Query -> Bool
executableMentions needle query = needle `T.isInfixOf` stripSqlComments (PGQ.getQueryText query)

sqlShapeSpec :: Spec
sqlShapeSpec = describe "introspection SQL no longer reads the comment catalogs" $ do
  it "Postgres reads neither pg_description nor col_description" $ do
    executableMentions "pg_description" (tableMetadata @'Vanilla) `shouldBe` False
    executableMentions "col_description" (tableMetadata @'Vanilla) `shouldBe` False

  it "Citus reads neither pg_description nor col_description" $ do
    executableMentions "pg_description" (tableMetadata @'Citus) `shouldBe` False
    executableMentions "col_description" (tableMetadata @'Citus) `shouldBe` False

  it "Cockroach reads neither pg_description nor col_description" $ do
    executableMentions "pg_description" (tableMetadata @'Cockroach) `shouldBe` False
    executableMentions "col_description" (tableMetadata @'Cockroach) `shouldBe` False

  it "the function metadata query reads neither pg_description nor obj_description" $ do
    executableMentions "pg_description" pgFunctionMetadataQuery `shouldBe` False
    executableMentions "obj_description" pgFunctionMetadataQuery `shouldBe` False

--------------------------------------------------------------------------------
-- 2. pgStripImportedDescriptions hook

tableName :: PG.QualifiedObject PG.TableName
tableName = PG.QualifiedObject PG.publicSchema (PG.TableName "test_table")

-- | A one-table, one-column introspection fixture with the given table and
-- column descriptions (as an older server would have produced from database
-- COMMENTs and persisted as stored introspection).
mkIntrospection ::
  Maybe PG.PGDescription ->
  Maybe G.Description ->
  DBObjectsIntrospection ('Postgres 'Vanilla)
mkIntrospection tableDesc columnDesc =
  DBObjectsIntrospection
    { _rsTables = HashMap.singleton tableName tableMeta,
      _rsFunctions = mempty,
      _rsScalars = mempty,
      _rsLogicalModels = mempty
    }
  where
    tableMeta =
      DBTableMetadata
        { _ptmiOid = OID 0,
          _ptmiColumns =
            [ RawColumnInfo
                { rciName = PG.unsafePGCol "test_column",
                  rciPosition = 1,
                  rciType = RawColumnTypeScalar PG.PGText,
                  rciIsNullable = True,
                  rciDescription = columnDesc,
                  rciMutability = ColumnMutability True True
                }
            ],
          _ptmiPrimaryKey = Nothing,
          _ptmiUniqueConstraints = mempty,
          _ptmiForeignKeys = mempty,
          _ptmiViewInfo = Nothing,
          _ptmiDescription = tableDesc,
          _ptmiExtraTableMetadata = PG.PGExtraTableMetadata STT.Table
        }

tableDescriptionOf :: DBObjectsIntrospection ('Postgres 'Vanilla) -> Maybe (Maybe Text)
tableDescriptionOf =
  fmap (fmap PG.getPGDescription . _ptmiDescription) . HashMap.lookup tableName . _rsTables

columnDescriptionsOf :: DBObjectsIntrospection ('Postgres 'Vanilla) -> Maybe [Maybe Text]
columnDescriptionsOf =
  fmap (map (fmap G.unDescription . rciDescription) . _ptmiColumns) . HashMap.lookup tableName . _rsTables

functionName :: PG.QualifiedObject PG.FunctionName
functionName = PG.QualifiedObject PG.publicSchema (PG.FunctionName "my_func")

-- | A two-overload function fixture (shape as produced by the function metadata
-- query), each overload carrying a distinct database comment, built via JSON so we
-- do not have to hand-construct every 'PGRawFunctionInfo' field.
functionOverloadsFixture :: FunctionOverloads ('Postgres 'Vanilla)
functionOverloadsFixture =
  either (\err -> error ("function fixture decode failed: " <> err)) id
    $ J.eitherDecode
    $ LBS.pack
    $ "[{\"oid\":1,\"has_variadic\":false,\"function_type\":\"VOLATILE\",\"return_type_schema\":\"public\",\"return_type_name\":\"integer\",\"return_type_type\":\"b\",\"returns_set\":false,\"input_arg_types\":[],\"input_arg_names\":[],\"default_args\":0,\"returns_table\":false,\"description\":\"first overload comment\"}"
      <> ",{\"oid\":2,\"has_variadic\":true,\"function_type\":\"STABLE\",\"return_type_schema\":\"public\",\"return_type_name\":\"text\",\"return_type_type\":\"b\",\"returns_set\":true,\"input_arg_types\":[],\"input_arg_names\":[],\"default_args\":0,\"returns_table\":false,\"description\":\"second overload comment\"}]"

functionDescriptionsOf :: DBObjectsIntrospection ('Postgres 'Vanilla) -> Maybe [Maybe Text]
functionDescriptionsOf =
  fmap (map (fmap PG.getPGDescription . PG.rfiDescription) . NE.toList . getFunctionOverloads)
    . HashMap.lookup functionName
    . _rsFunctions

-- | The (returnsSet, returnTypeName-as-shown) of each overload, to check that
-- structural signature fields survive the description strip.
functionSignaturesOf :: DBObjectsIntrospection ('Postgres 'Vanilla) -> Maybe [Bool]
functionSignaturesOf =
  fmap (map PG.rfiReturnsSet . NE.toList . getFunctionOverloads)
    . HashMap.lookup functionName
    . _rsFunctions

hookSpec :: Spec
hookSpec = describe "pgStripImportedDescriptions" $ do
  let fixture =
        (mkIntrospection (Just (PG.PGDescription "a table comment")) (Just (G.Description "a column comment")))
          { _rsFunctions = HashMap.singleton functionName functionOverloadsFixture
          }
      stripped = pgStripImportedDescriptions fixture

  it "sanity: the fixture starts with descriptions on both overloads" $ do
    tableDescriptionOf fixture `shouldBe` Just (Just "a table comment")
    functionDescriptionsOf fixture `shouldBe` Just [Just "first overload comment", Just "second overload comment"]

  it "drops the table description" $
    tableDescriptionOf stripped `shouldBe` Just Nothing

  it "drops column descriptions" $
    columnDescriptionsOf stripped `shouldBe` Just [Nothing]

  it "drops the description of every function overload" $
    functionDescriptionsOf stripped `shouldBe` Just [Nothing, Nothing]

  it "preserves structural function signature fields of every overload" $
    functionSignaturesOf stripped `shouldBe` Just [False, True]

  it "keeps the table and function entries" $ do
    HashMap.size (_rsTables stripped) `shouldBe` 1
    HashMap.size (_rsFunctions stripped) `shouldBe` 1

  it "is idempotent" $ do
    tableDescriptionOf (pgStripImportedDescriptions stripped) `shouldBe` Just Nothing
    functionDescriptionsOf (pgStripImportedDescriptions stripped) `shouldBe` Just [Nothing, Nothing]

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  sqlShapeSpec
  hookSpec
