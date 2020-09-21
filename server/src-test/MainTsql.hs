-- | Tests for Tsql conversion.

module Main where

import           Data.Functor.Identity
import qualified Data.List.NonEmpty as NE
import           Data.Proxy
import           Data.String
import qualified Database.ODBC.SQLServer as Odbc
import qualified Hasura.SQL.Tsql.FromIr as FromIr
import           Hasura.SQL.Tsql.ToQuery as ToQuery
import           Hasura.SQL.Tsql.Types as Tsql
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
            (ToQuery.fromExpression (ValueExpression (Odbc.BoolValue bool)))
            (Odbc.toSql bool)))
  it
    "Sanity check"
    (shouldBe
       (Odbc.renderQuery
          (fromSelect
             Select
               { selectTop = Top 1
               , selectProjections =
                   NE.fromList
                     [ ExpressionProjection
                         Aliased
                           { aliasedThing =
                               Tsql.ValueExpression (Odbc.BoolValue True)
                           , aliasedAlias =
                               Just (Alias {aliasText = "column_alias"})
                           }
                     ]
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
                       , aliasedAlias = Just (Alias {aliasText = "alias"})
                       }
               }))
       "SELECT\n\
       \TOP 1\n\
       \1 AS [column_alias]\n\
       \FROM\n\
       \[schema].[table] AS [alias]")

--------------------------------------------------------------------------------
-- Tests that require a database connection

connectedTests :: SpecWith Odbc.Connection
connectedTests = sanity

sanity :: SpecWith Odbc.Connection
sanity = do
  it
    "Query sanity check"
    (\connection ->
       shouldReturn (Odbc.query connection "select 1") [Identity (1 :: Int)])
  it
    "SELECT TOP 1 1"
    (\connection ->
       shouldReturn (Odbc.query connection "SELECT TOP 1 1") [Identity (1 :: Int)])
