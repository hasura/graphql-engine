-- | Tests for Tsql conversion.

module Main where

import           Control.Monad.Validate
import           Data.Functor.Identity
import qualified Data.List.NonEmpty as NE
import           Data.String
import qualified Database.ODBC.SQLServer as Odbc
import qualified Hasura.RQL.DML.Select.Types as Ir
import qualified Hasura.RQL.Types.BoolExp as Ir
import qualified Hasura.RQL.Types.Column as Ir
import           Hasura.SQL.Tsql.FromIr as FromIr
import           Hasura.SQL.Tsql.ToQuery as ToQuery
import           Hasura.SQL.Tsql.Types as Tsql
import qualified Hasura.SQL.Types as Sql
import qualified Language.GraphQL.Draft.Syntax as G
import           Prelude
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck (property)

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

-- Notes:
-- In asnFields below, we have: Ir.AnnFieldG Hasura.SQL.DML.SQLExp
-- So it seems that the SQLExp from SQL is mixed freely with RQL before we hit
-- mkSQLSelect :: JsonAggSelect -> AnnSimpleSel -> S.Select
-- which is odd.
--
-- More notes:
--
-- The column field requires PG column info.
--
-- data AnnFieldG v
--   = AFColumn !AnnColumnField
--   | AFObjectRelation !(ObjectRelationSelectG v)
--   | AFArrayRelation !(ArraySelectG v)
--   | AFComputedField !(ComputedFieldSelect v)
--   | AFRemote !RemoteSelect
--   | AFNodeId !QualifiedTable !PrimaryKeyColumns
--   | AFExpression !T.Text
--   deriving (Show, Eq)
--
-- mkAnnColumnField :: PGColumnInfo -> Maybe ColumnOp -> AnnFieldG v
--
-- mkAnnColumnFieldAsText :: PGColumnInfo -> AnnFieldG v
--
fromIrTests :: Spec
fromIrTests = do
  it
    "Select: sanity property test"
    (property
       (\limit fieldText fieldAlias schemaName tableName colName ->
          shouldBe
            (runValidate
               (FromIr.runFromIr
                  (FromIr.fromSelectFields
                     Ir.AnnSelectG
                       { _asnFields =
                           [ ( fromString fieldAlias
                             , Ir.AFExpression (fromString fieldText))
                           ]
                       , _asnFrom =
                           Ir.FromTable
                             Sql.QualifiedObject
                               { qSchema = Sql.SchemaName schemaName
                               , qName = Sql.TableName tableName
                               }
                       , _asnPerm =
                           Ir.TablePerm
                             { _tpFilter =
                                 Ir.BoolFld
                                   (Ir.AVCol
                                      (Ir.PGColumnInfo
                                         { pgiColumn = Sql.unsafePGCol colName
                                         , pgiName = G.unsafeMkName colName
                                         , pgiPosition = 0
                                         , pgiType =
                                             Ir.PGColumnScalar Sql.PGSmallInt
                                         , pgiIsNullable = False
                                         , pgiDescription = Nothing
                                         })
                                      [Ir.ANISNULL])
                             , _tpLimit = Just limit
                             }
                       , _asnArgs =
                           Ir.SelectArgs
                             { _saWhere = Nothing
                             , _saOrderBy = Nothing
                             , _saLimit = Nothing
                             , _saOffset = Nothing
                             , _saDistinct = Nothing
                             }
                       , _asnStrfyNum = False
                       })))
            (Right
               Select
                 { selectTop =
                     Commented
                       { commentedComment = Just DueToPermission
                       , commentedThing = Top limit
                       }
                 , selectProjections =
                     NE.fromList
                       [ ExpressionProjection
                           Aliased
                             { aliasedThing =
                                 Tsql.ValueExpression
                                   (Odbc.TextValue (fromString fieldText))
                             , aliasedAlias =
                                 Just
                                   (Alias {aliasText = fromString fieldAlias})
                             }
                       ]
                 , selectFrom =
                     FromQualifiedTable
                       Aliased
                         { aliasedThing =
                             Qualified
                               { qualifiedThing =
                                   TableName {tableNameText = tableName}
                               , qualifiedSchemaName =
                                   Just
                                     (SchemaName
                                        {schemaNameParts = [schemaName]})
                               }
                         , aliasedAlias = Nothing
                         }
                 , selectWhere =
                     ExpressionWhere
                       (AndExpression
                          [IsNullExpression (ColumnExpression colName)])
                 })))

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
          (ToQuery.fromSelect
             Select
               { selectTop =
                   Commented
                     { commentedComment = pure DueToPermission
                     , commentedThing = Top 1
                     }
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
               , selectWhere = NoWhere
               }))
       "SELECT\n\
       \TOP 1 /* Due to permission */ \n\
       \1 AS [column_alias]\n\
       \FROM\n\
       \[schema].[table] AS [alias]\n")

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
    "SELECT TOP 1 3"
    (\connection ->
       shouldReturn (Odbc.query connection "SELECT TOP 1 3") [Identity (3 :: Int)])
