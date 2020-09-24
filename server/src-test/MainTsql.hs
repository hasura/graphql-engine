-- | Tests for Tsql conversion.

module Main (main) where

import           Control.Monad.Validate
import           Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.String
import qualified Database.ODBC.SQLServer as Odbc
import qualified Hasura.RQL.DML.Select.Types as Ir
import qualified Hasura.RQL.Types.BoolExp as Ir
import qualified Hasura.RQL.Types.Column as Ir
import qualified Hasura.RQL.Types.Common as Ir
import qualified Hasura.SQL.DML as Sql
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
  tracks_id
  tracks_album_title
  albums_tracks_id
  tracks_aggregate_count

tracks_id :: SpecWith ()
tracks_id =
  it
    "tracks { id }"
    (shouldBe
       (runValidate
          (FromIr.runFromIr
             (FromIr.fromSelectRows
                Ir.AnnSelectG
                  { _asnFields =
                      [ ( "id"
                        , Ir.AFColumn
                            (Ir.AnnColumnField
                               { _acfInfo =
                                   Ir.PGColumnInfo
                                     { pgiColumn = "id"
                                     , pgiName = G.unsafeMkName "id"
                                     , pgiPosition = 1
                                     , pgiType = Ir.PGColumnScalar Sql.PGInteger
                                     , pgiIsNullable = False
                                     , pgiDescription = Nothing
                                     }
                               , _acfAsText = False
                               , _acfOp = Nothing
                               }))
                      ]
                  , _asnFrom =
                      Ir.FromTable
                        (Sql.QualifiedObject
                           {qSchema = "public", qName = "tracks"})
                  , _asnPerm =
                      Ir.TablePerm
                        {_tpFilter = Ir.BoolAnd [], _tpLimit = Nothing}
                  , Ir._asnArgs =
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
          (Select
             { selectTop =
                 Commented {commentedComment = Nothing, commentedThing = NoTop}
             , selectProjections =
                 FieldNameProjection
                   (Aliased
                      { aliasedThing = FieldName {fieldNameText = "id"}
                      , aliasedAlias = Just (Alias {aliasText = "id"})
                      }) :|
                 []
             , selectFrom =
                 FromQualifiedTable
                   (Aliased
                      { aliasedThing =
                          Qualified
                            { qualifiedThing =
                                TableName {tableNameText = "tracks"}
                            , qualifiedSchemaName =
                                Just (SchemaName {schemaNameParts = ["public"]})
                            }
                      , aliasedAlias = Nothing
                      })
             , selectJoins = []
             , selectWhere = ExpressionWhere (AndExpression [])
             })))

tracks_album_title :: SpecWith ()
tracks_album_title =
  it
    "tracks { album { title } }"
    (shouldBe
       (runValidate
          (FromIr.runFromIr
             (FromIr.fromSelectRows
                Ir.AnnSelectG
                  { _asnFields =
                      [ ( "album"
                        , Ir.AFObjectRelation
                            (Ir.AnnRelationSelectG
                               { aarRelationshipName =
                                   Ir.RelName (Ir.mkNonEmptyTextUnsafe "album")
                               , aarColumnMapping =
                                   HM.fromList [("album_id", "id")]
                               , aarAnnSelect =
                                   Ir.AnnObjectSelectG
                                     { _aosFields =
                                         [ ( "title"
                                           , Ir.AFColumn
                                               (Ir.AnnColumnField
                                                  { _acfInfo =
                                                      Ir.PGColumnInfo
                                                        { pgiColumn = "title"
                                                        , pgiName =
                                                            G.unsafeMkName
                                                              "title"
                                                        , pgiPosition = 2
                                                        , pgiType =
                                                            Ir.PGColumnScalar
                                                              Sql.PGVarchar
                                                        , pgiIsNullable = False
                                                        , pgiDescription =
                                                            Nothing
                                                        }
                                                  , _acfAsText = False
                                                  , _acfOp = Nothing
                                                  }))
                                         ]
                                     , _aosTableFrom =
                                         Sql.QualifiedObject
                                           { qSchema = "public"
                                           , qName = "albums"
                                           }
                                     , _aosTableFilter = Ir.BoolAnd []
                                     }
                               }))
                      ]
                  , _asnFrom =
                      Ir.FromTable
                        (Sql.QualifiedObject
                           {qSchema = "public", qName = "tracks"})
                  , _asnPerm =
                      Ir.TablePerm
                        {_tpFilter = Ir.BoolAnd [], _tpLimit = Nothing}
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
          (Select
             { selectTop =
                 Commented {commentedComment = Nothing, commentedThing = NoTop}
             , selectProjections =
                 FieldNameProjection
                   (Aliased
                      { aliasedThing = FieldName {fieldNameText = "album"}
                      , aliasedAlias = Just (Alias {aliasText = "album"})
                      }) :|
                 []
             , selectFrom =
                 FromQualifiedTable
                   (Aliased
                      { aliasedThing =
                          Qualified
                            { qualifiedThing =
                                TableName {tableNameText = "tracks"}
                            , qualifiedSchemaName =
                                Just (SchemaName {schemaNameParts = ["public"]})
                            }
                      , aliasedAlias = Nothing
                      })
             , selectJoins =
                 [ Join
                     { joinSelect =
                         Select
                           { selectTop =
                               Commented
                                 { commentedComment = Nothing
                                 , commentedThing = NoTop
                                 }
                           , selectProjections =
                               FieldNameProjection
                                 (Aliased
                                    { aliasedThing =
                                        FieldName {fieldNameText = "title"}
                                    , aliasedAlias =
                                        Just (Alias {aliasText = "title"})
                                    }) :|
                               []
                           , selectFrom =
                               FromQualifiedTable
                                 (Aliased
                                    { aliasedThing =
                                        Qualified
                                          { qualifiedThing =
                                              TableName
                                                {tableNameText = "albums"}
                                          , qualifiedSchemaName =
                                              Just
                                                (SchemaName
                                                   { schemaNameParts =
                                                       ["public"]
                                                   })
                                          }
                                    , aliasedAlias = Nothing
                                    })
                           , selectJoins = []
                           , selectWhere = ExpressionWhere (AndExpression [])
                           }
                     , joinFieldName = FieldName {fieldNameText = "album"}
                     }
                 ]
             , selectWhere = ExpressionWhere (AndExpression [])
             })))

tracks_aggregate_count :: SpecWith ()
tracks_aggregate_count =
  it
    "tracks_aggregate { aggregate { count } }"
    (shouldBe
       (runValidate
          (FromIr.runFromIr
             (FromIr.fromSelectAggregate
                Ir.AnnSelectG
                  { _asnFields =
                      [ ( "aggregate"
                        , Ir.TAFAgg [("count", Ir.AFCount Sql.CTStar)])
                      ]
                  , _asnFrom =
                      Ir.FromTable
                        (Sql.QualifiedObject
                           {qSchema = "public", qName = "tracks"})
                  , _asnPerm =
                      Ir.TablePerm
                        {_tpFilter = Ir.BoolAnd [], _tpLimit = Nothing}
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
          (Select
             { selectTop =
                 Commented {commentedComment = Nothing, commentedThing = NoTop}
             , selectProjections =
                 AggregateProjection
                   (Aliased
                      { aliasedThing = CountAggregate StarCountable
                      , aliasedAlias = Just (Alias {aliasText = "count"})
                      }) :|
                 []
             , selectFrom =
                 FromQualifiedTable
                   (Aliased
                      { aliasedThing =
                          Qualified
                            { qualifiedThing =
                                TableName {tableNameText = "tracks"}
                            , qualifiedSchemaName =
                                Just (SchemaName {schemaNameParts = ["public"]})
                            }
                      , aliasedAlias = Nothing
                      })
             , selectJoins = []
             , selectWhere = ExpressionWhere (AndExpression [])
             })))

albums_tracks_id :: SpecWith ()
albums_tracks_id =
  it
    "tracks { album { tracks { id } } }"
    (do shouldBe
          (runValidate
             (FromIr.runFromIr
                (FromIr.fromSelectRows
                   Ir.AnnSelectG
                     { _asnFields =
                         [ ( "tracks"
                           , Ir.AFArrayRelation
                               (Ir.ASSimple
                                  (Ir.AnnRelationSelectG
                                     { aarRelationshipName =
                                         Ir.RelName
                                           (Ir.mkNonEmptyTextUnsafe "tracks")
                                     , aarColumnMapping =
                                         HM.fromList [("id", "album_id")]
                                     , aarAnnSelect =
                                         Ir.AnnSelectG
                                           { _asnFields =
                                               [ ( "id"
                                                 , Ir.AFColumn
                                                     (Ir.AnnColumnField
                                                        { _acfInfo =
                                                            Ir.PGColumnInfo
                                                              { pgiColumn = "id"
                                                              , pgiName =
                                                                  G.unsafeMkName
                                                                    "id"
                                                              , pgiPosition = 1
                                                              , pgiType =
                                                                  Ir.PGColumnScalar
                                                                    Sql.PGInteger
                                                              , pgiIsNullable =
                                                                  False
                                                              , pgiDescription =
                                                                  Nothing
                                                              }
                                                        , _acfAsText = False
                                                        , _acfOp = Nothing
                                                        }))
                                               ]
                                           , _asnFrom =
                                               Ir.FromTable
                                                 (Sql.QualifiedObject
                                                    { qSchema = "public"
                                                    , qName = "tracks"
                                                    })
                                           , _asnPerm =
                                               Ir.TablePerm
                                                 { _tpFilter = Ir.BoolAnd []
                                                 , _tpLimit = Nothing
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
                                           }
                                     })))
                         ]
                     , _asnFrom =
                         Ir.FromTable
                           (Sql.QualifiedObject
                              {qSchema = "public", qName = "albums"})
                     , _asnPerm =
                         Ir.TablePerm
                           {_tpFilter = Ir.BoolAnd [], _tpLimit = Nothing}
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
             (Select
                { selectTop =
                    Commented
                      {commentedComment = Nothing, commentedThing = NoTop}
                , selectProjections =
                    FieldNameProjection
                      (Aliased
                         { aliasedThing = FieldName {fieldNameText = "tracks"}
                         , aliasedAlias = Just (Alias {aliasText = "tracks"})
                         }) :|
                    []
                , selectFrom =
                    FromQualifiedTable
                      (Aliased
                         { aliasedThing =
                             Qualified
                               { qualifiedThing =
                                   TableName {tableNameText = "albums"}
                               , qualifiedSchemaName =
                                   Just
                                     (SchemaName {schemaNameParts = ["public"]})
                               }
                         , aliasedAlias = Nothing
                         })
                , selectJoins =
                    [ Join
                        { joinSelect =
                            Select
                              { selectTop =
                                  Commented
                                    { commentedComment = Nothing
                                    , commentedThing = NoTop
                                    }
                              , selectProjections =
                                  FieldNameProjection
                                    (Aliased
                                       { aliasedThing =
                                           FieldName {fieldNameText = "id"}
                                       , aliasedAlias =
                                           Just (Alias {aliasText = "id"})
                                       }) :|
                                  []
                              , selectFrom =
                                  FromQualifiedTable
                                    (Aliased
                                       { aliasedThing =
                                           Qualified
                                             { qualifiedThing =
                                                 TableName
                                                   {tableNameText = "tracks"}
                                             , qualifiedSchemaName =
                                                 Just
                                                   (SchemaName
                                                      { schemaNameParts =
                                                          ["public"]
                                                      })
                                             }
                                       , aliasedAlias = Nothing
                                       })
                              , selectJoins = []
                              , selectWhere = ExpressionWhere (AndExpression [])
                              }
                        , joinFieldName = FieldName {fieldNameText = "tracks"}
                        }
                    ]
                , selectWhere = ExpressionWhere (AndExpression [])
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
               { selectJoins = []
               , selectTop =
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
