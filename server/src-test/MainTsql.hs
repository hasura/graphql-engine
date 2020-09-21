-- | Tests for Tsql conversion.

module Main where

import           Data.Functor.Identity
import           Data.Proxy
import           Data.String
import qualified Database.ODBC.SQLServer as Odbc
import qualified Hasura.SQL.Tsql.FromIr as FromIr
import Hasura.SQL.Tsql.ToQuery as ToQuery
import Hasura.SQL.Tsql.Types as Tsql
import           Prelude
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = hspec spec

connect :: IO Odbc.Connection
connect = do
  connectionString <- getEnv "CONNSTR"
  Odbc.connect (fromString connectionString)

--------------------------------------------------------------------------------
-- Main tests declaration

spec :: SpecWith ()
spec = do
  describe "Pure tests" pureTests
  describe
    "Connected tests"
    (beforeAll connect (afterAll Odbc.close connectedTests))

--------------------------------------------------------------------------------
-- Tests that are pure and do not require any I/O

pureTests :: Spec
pureTests = do
  describe "IR to Tsql" fromIrTests
  describe "Tsql to Query" toQueryTests

fromIrTests :: Spec
fromIrTests = do
  it
    "Select"
    (shouldBe
       (runIdentity (FromIr.runFromIr (FromIr.fromSelect Proxy)))
       Proxy)
  it
    "Expression"
    (shouldBe
       (runIdentity (FromIr.runFromIr (FromIr.fromExpression Proxy)))
       Proxy)

--------------------------------------------------------------------------------
-- Tests for converting from the Tsql AST to a Query

toQueryTests :: Spec
toQueryTests = do
  it
    "Boolean"
    (property
       (\bool ->
          shouldBe
            (ToQuery.fromExpression (Tsql.ValueExpression (Odbc.BoolValue bool)))
            (Odbc.toSql bool)))
  it
    "Sanity check"
    (shouldBe
       (Odbc.renderQuery
          (fromSelect
             Select
               { selectTop = Top 1
               , selectExpression = ValueExpression (Odbc.BoolValue True)
               , selectFrom =
                   FromQualifiedTable
                     Aliased
                       { aliasedThing =
                           Qualified
                             { qualifiedThing =
                                 TableName {tableNameText = "table"}
                             , qualifiedSchemaName =
                                 Just
                                   (SchemaName {schemaNameParts = ["schema"]})
                             }
                       , aliasedColumnAlias =
                           Just (ColumnAlias {columnAliasText = "alias"})
                       }
               }))
       "SELECT\nTOP 1\n1\nFROM\n[schema].[table] AS [alias]")

--------------------------------------------------------------------------------
-- Tests that require a database connection

connectedTests :: SpecWith Odbc.Connection
connectedTests = sanity

sanity :: SpecWith Odbc.Connection
sanity =
  it
    "Query sanity check"
    (\connection ->
       shouldReturn (Odbc.query connection "select 1") [Identity (1 :: Int)])
