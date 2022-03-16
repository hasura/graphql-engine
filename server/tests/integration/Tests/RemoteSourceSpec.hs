{-# LANGUAGE QuasiQuotes #-}

-- | Tests for DB-to-DB joins.
module Tests.RemoteSourceSpec
  ( spec,
  )
where

import Backend.Postgres (withPostgres)
import Control.Exception (throwIO)
import Data.Aeson (ToJSON (..), Value (..), toJSON)
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict.InsOrd.Extended qualified as OM
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.NonEmpty (mkNonEmptyTextUnsafe)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Relationships.ToSource
import Hasura.RQL.Types.SourceCustomization (emptySourceCustomization)
import Hasura.RQL.Types.Table (emptyTableConfig)
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Hasura.SQL.Backend (BackendType (..), PostgresKind (..))
import Hasura.Server.Utils (userIdHeader, userRoleHeader)
import Hasura.Session (mkRoleName)
import Hasura.Test.App (withHasuraTestApp)
import Hasura.Test.Requests (postgresRunSql, replaceMetadata, v1graphql)
import Network.Wai (Application)
import Test.Hspec (Spec, SpecWith, aroundAll, describe, it)
import Test.Hspec.Wai (getState, pendingWith, shouldRespondWith)
import Test.Hspec.Wai.JSON (json)
import Tests.RemoteSourceSql
  ( RemoteSourceSql (..),
    albums,
    artist_id,
    artist_id_null,
    artist_name,
    artists,
    fav_album,
    id_,
    id_null,
    name,
    remoteSourceSql_postgres,
    title,
  )
import Text.Shakespeare.Text (st)

--------------------------------------------------------------------------------
-- Tests

spec :: Spec
spec =
  aroundAll withMetadata $ do
    setup remoteSourceSql_postgres
    remoteSourcesSpec

withMetadata ::
  ((SourceMetadata ('Postgres 'Vanilla), Application) -> IO b) -> IO b
withMetadata action = do
  result <- fmap join . withPostgres $ \metadataDbUrl _ ->
    withPostgres $ \_ initialSourceMetadata ->
      withHasuraTestApp metadataDbUrl $ \hasuraApp ->
        liftIO $ action (initialSourceMetadata, hasuraApp)
  onLeft result throwIO

remoteSourcesSpec :: SpecWith (SourceMetadata b, Application)
remoteSourcesSpec = describe "DB-to-DB joins tests" $ do
  remoteSourcesBasicSpec
  remoteSourcesSchemaSpec
  remoteSourcesPermissionsSpec
  remoteSourcesExecutionSpec

-- | Basic queries using DB-to-DB joins
remoteSourcesBasicSpec :: SpecWith (SourceMetadata b, Application)
remoteSourcesBasicSpec = describe "Basic" $ do
  it "Self-relationship" $ do
    let query =
          T.unpack
            [st|
            query {
              #{albums} {
                #{albums} {
                  #{title}
                }
              }
            }
          |]
        expectedResponse =
          [json|
            {
              "data": {
                "albums": [
                  {
                    "albums": {
                      "title": "Album 1A"
                    }
                  },
                  {
                    "albums": {
                      "title": "Album 1B"
                    }
                  },
                  {
                    "albums": {
                      "title": "Album 2A"
                    }
                  },
                  {
                    "albums": {
                      "title": "Album 2B"
                    }
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Mutual relationships (1)" $ do
    let query =
          T.unpack
            [st|
            query {
              #{albums} {
                #{title}
                #{artists} {
                  #{id_}
                  #{albums} {
                    #{title}
                  }
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "albums": [
                  {
                    "title": "Album 1A",
                    "artists": {
                      "id": 1,
                      "albums": [
                        {
                          "title": "Album 1A"
                        },
                        {
                          "title": "Album 1B"
                        }
                      ]
                    }
                  },
                  {
                    "title": "Album 1B",
                    "artists": {
                      "id": 1,
                      "albums": [
                        {
                          "title": "Album 1A"
                        },
                        {
                          "title": "Album 1B"
                        }
                      ]
                    }
                  },
                  {
                    "title": "Album 2A",
                    "artists": {
                      "id": 2,
                      "albums": [
                        {
                          "title": "Album 2A"
                        },
                        {
                          "title": "Album 2B"
                        }
                      ]
                    }
                  },
                  {
                    "title": "Album 2B",
                    "artists": {
                      "id": 2,
                      "albums": [
                        {
                          "title": "Album 2A"
                        },
                        {
                          "title": "Album 2B"
                        }
                      ]
                    }
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Mutual relationships (2)" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{id_}
                #{albums} {
                  #{title}
                  #{artists} {
                    #{id_}
                  }
                }
              }
            }
          |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "id": 1,
                    "albums": [
                      {
                        "title": "Album 1A",
                        "artists": {
                          "id": 1
                        }
                      },
                      {
                        "title": "Album 1B",
                        "artists": {
                          "id": 1
                        }
                      }
                    ]
                  },
                  {
                    "id": 2,
                    "albums": [
                      {
                        "title": "Album 2A",
                        "artists": {
                          "id": 2
                        }
                      },
                      {
                        "title": "Album 2B",
                        "artists": {
                          "id": 2
                        }
                      }
                    ]
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Join over multiple fields (array relationship)" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{id_}
                favAlbums {
                  #{title}
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "id": 1,
                    "favAlbums": [
                      {
                        "title": "Album 1A"
                      }
                    ]
                  },
                  {
                    "id": 2,
                    "favAlbums": [
                      {
                        "title": "Album 2A"
                      }
                    ]
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Join over multiple fields (object relationship)" $ do
    let query =
          T.unpack
            [st|
            query {
              #{albums} {
                #{title}
                favouriteOf {
                  #{id_}
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "albums": [
                  {
                    "title": "Album 1A",
                    "favouriteOf": {
                      "id": 1
                    }
                  },
                  {
                    "title": "Album 1B",
                    "favouriteOf": null
                  },
                  {
                    "title": "Album 2A",
                    "favouriteOf": {
                      "id": 2
                    }
                  },
                  {
                    "title": "Album 2B",
                    "favouriteOf": null
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

-- | Tests for schema generation for DB-to-DB joins
--
-- We test presence or absence of fields in the schema by querying the fields
-- expecting success or a particular error, respectively.
--
-- Note: The presence of the 'distinct_on', 'where', 'order_by', 'limit', and
-- 'offset' fields in array relationships is tested as a part of the execution
-- tests. See `remoteSourcesExecutionArrRelSpec`.
remoteSourcesSchemaSpec :: SpecWith (SourceMetadata b, Application)
remoteSourcesSchemaSpec = describe "Schema generation" $ do
  it "Object relationships should not be present in 'bool_exp'" $ do
    let query =
          T.unpack
            [st|
            query {
              #{albums}(where: {#{artists}: {#{id_}: {_gt: 1}}}) {
                #{title}
              }
            }
          |]
        expectedResponse =
          [json|
            {
              "errors": [
                {
                  "extensions": {
                    "path": "$.selectionSet.albums.args.where.artists",
                    "code": "validation-failed"
                  },
                  "message": "field \"artists\" not found in type: 'albums_bool_exp'"
                }
              ]
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Object relationships should not be present in 'order_by'" $ do
    let query =
          T.unpack
            [st|
            query {
              #{albums}(order_by: {#{artists}: {#{id_}: asc}}) {
                #{title}
              }
            }
          |]
        expectedResponse =
          [json|
            {
              "errors": [
                {
                  "extensions": {
                    "path": "$.selectionSet.albums.args.order_by[0].artists",
                    "code": "validation-failed"
                  },
                  "message": "field \"artists\" not found in type: 'albums_order_by'"
                }
              ]
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Array relationships should not be present in 'bool_exp'" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists}(where: {#{albums}: {#{id_}: {_gt: 1}}}) {
                #{name}
              }
            }
          |]
        expectedResponse =
          [json|
            {
              "errors": [
                {
                  "extensions": {
                    "path": "$.selectionSet.artists.args.where.albums",
                    "code": "validation-failed"
                  },
                  "message": "field \"albums\" not found in type: 'artists_bool_exp'"
                }
              ]
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Array relationships should not be present in 'order_by'" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists}(order_by: {#{albums}_aggregate: {count: asc}}) {
                #{name}
              }
            }
          |]
        expectedResponse =
          [json|
            {
              "errors": [
                {
                  "extensions": {
                    "path": "$.selectionSet.artists.args.order_by[0].albums_aggregate",
                    "code": "validation-failed"
                  },
                  "message": "field \"albums_aggregate\" not found in type: 'artists_order_by'"
                }
              ]
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

-- | Tests for the interaction of permissions and DB-to-DB joins
remoteSourcesPermissionsSpec :: SpecWith (SourceMetadata b, Application)
remoteSourcesPermissionsSpec = describe "Permissions" $ do
  it "Object relationships respect the row permissions of the target" $ do
    let query =
          T.unpack
            [st|
            query {
              #{albums} {
                #{title}
                #{artists} {
                  #{id_}
                }
              }
            }
          |]
        headers =
          [ (userRoleHeader, "objRel-user"),
            (userIdHeader, "1")
          ]
        expectedResponse =
          [json|
            {
              "data": {
                "albums": [
                  {
                    "title": "Album 1A",
                    "artists": {
                      "id": 1
                    }
                  },
                  {
                    "title": "Album 1B",
                    "artists": {
                      "id": 1
                    }
                  },
                  {
                    "title": "Album 2A",
                    "artists": null
                  },
                  {
                    "title": "Album 2B",
                    "artists": null
                  }
                ]
              }
            }
          |]
    v1graphql headers query `shouldRespondWith` expectedResponse

  it "Object relationships respect the column permissions of the target" $ do
    let query =
          T.unpack
            [st|
            query {
              #{albums} {
                #{artists} {
                  #{name}
                }
              }
            }
          |]
        headers =
          [ (userRoleHeader, "objRel-user"),
            (userIdHeader, "1")
          ]
        expectedResponse =
          [json|
            {
              "errors": [
                {
                  "extensions": {
                    "path": "$.selectionSet.albums.selectionSet.artists.selectionSet.name",
                    "code": "validation-failed"
                  },
                  "message": "field \"name\" not found in type: 'artists'"
                }
              ]
            }
          |]
    v1graphql headers query `shouldRespondWith` expectedResponse

  it "Object relationships are not visible without select permissions on the target table" $ do
    let query =
          T.unpack
            [st|
            query {
              #{albums} {
                #{artist_id}
                #{artists} {
                  #{id_}
                }
              }
            }
          |]
        headers = [(userRoleHeader, "only-albums")]
        expectedResponse =
          [json|
            {
              "errors": [
                {
                  "extensions": {
                    "path": "$.selectionSet.albums.selectionSet.artists",
                    "code": "validation-failed"
                  },
                  "message": "field \"artists\" not found in type: 'albums'"
                }
              ]
            }
          |]
    v1graphql headers query `shouldRespondWith` expectedResponse

  it "Array relationships respect the row permissions of the target" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{id_}
                #{albums} {
                  #{artist_id}
                }
              }
            }
          |]
        headers =
          [ (userRoleHeader, "arrRel-user"),
            (userIdHeader, "1")
          ]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "id": 1,
                    "albums": [
                      {
                        "artist_id": 1
                      },
                      {
                        "artist_id": 1
                      }
                    ]
                  },
                  {
                    "id": 2,
                    "albums": []
                  }
                ]
              }
            }
          |]
    v1graphql headers query `shouldRespondWith` expectedResponse

  it "Array relationships respect the column permissions of the target" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{id_}
                #{albums} {
                  #{title}
                }
              }
            }
          |]
        headers =
          [ (userRoleHeader, "arrRel-user"),
            (userIdHeader, "1")
          ]
        expectedResponse =
          [json|
            {
              "errors": [
                {
                  "extensions": {
                    "path": "$.selectionSet.artists.selectionSet.albums.selectionSet.title",
                    "code": "validation-failed"
                  },
                  "message": "field \"title\" not found in type: 'albums'"
                }
              ]
            }
          |]
    v1graphql headers query `shouldRespondWith` expectedResponse

  it "Array relationships respect the aggregation permissions of the target" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{albums}_aggregate {
                  aggregate {
                    count
                  }
                }
              }
            }
          |]
        headers =
          [ (userRoleHeader, "arrRel-user"),
            (userIdHeader, "1")
          ]
        expectedResponse =
          [json|
            {
              "errors": [
                {
                  "extensions": {
                    "path": "$.selectionSet.artists.selectionSet.albums_aggregate",
                    "code": "validation-failed"
                  },
                  "message": "field \"albums_aggregate\" not found in type: 'artists'"
                }
              ]
            }
          |]
    v1graphql headers query `shouldRespondWith` expectedResponse

  it "Array relationships are not visible without select permissions on the target table" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{id_}
                #{albums} {
                  #{id_}
                }
              }
            }
          |]
        headers = [(userRoleHeader, "only-artists")]
        expectedResponse =
          [json|
            {
              "errors": [
                {
                  "extensions": {
                    "path": "$.selectionSet.artists.selectionSet.albums",
                    "code": "validation-failed"
                  },
                  "message": "field \"albums\" not found in type: 'artists'"
                }
              ]
            }
          |]
    v1graphql headers query `shouldRespondWith` expectedResponse

-- | Tests for specific behaviours of DB-to-DB joins
remoteSourcesExecutionSpec :: SpecWith (SourceMetadata b, Application)
remoteSourcesExecutionSpec = describe "Execution" $ do
  remoteSourcesExecutionNullValuesSpec
  remoteSourcesExecutionIncompatColsSpec
  remoteSourcesExecutionArrRelSpec

-- | Tests for DB-to-DB joins behaviour in the presence of null values
remoteSourcesExecutionNullValuesSpec :: SpecWith (SourceMetadata b, Application)
remoteSourcesExecutionNullValuesSpec = describe "Null values" $ do
  it "An object relationship returning a row with null for any join column resolves to null" $ do
    let query =
          T.unpack
            [st|
            query {
              #{albums} {
                artists_null {
                  #{name}
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "albums": [
                  {
                    "artists_null": {
                      "name": "Author"
                    }
                  },
                  {
                    "artists_null": {
                      "name": "Author"
                    }
                  },
                  {
                    "artists_null": null
                  },
                  {
                    "artists_null": null
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "An object relationship with more than one match returns null" $ do
    pendingWith "Instead of null, one of the multiple matches is returned."
    let query =
          T.unpack
            [st|
            query {
              #{albums} {
                #{title}
                ambiguous_artist {
                  #{id_}
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "albums": [
                  {
                    "title": "Album 1A",
                    "ambiguous_artist": null
                  },
                  {
                    "title": "Album 1B",
                    "ambiguous_artist": null
                  },
                  {
                    "title": "Album 2A",
                    "ambiguous_artist": null
                  },
                  {
                    "title": "Album 2B",
                    "ambiguous_artist": null
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "An array relationship excludes rows with null values in any join column" $ do
    pendingWith $ "Should we return '[]' if any join column of /any/ row is null?"
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                albums_null {
                  #{title}
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "albums_null": [
                      {
                        "title": "Album 1A"
                      }
                    ]
                  },
                  {
                    "albums_null": [
                      {
                        "title": "Album 2A"
                      }
                    ]
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

-- | Tests for DB-to-DB joins behaviour in the presence of incompatible columns
remoteSourcesExecutionIncompatColsSpec :: SpecWith (SourceMetadata b, Application)
remoteSourcesExecutionIncompatColsSpec = describe "Incompatible columns" $ do
  it "Incompatible join columns (different data types) cause a runtime error" $ do
    pendingWith "Should we prevent such relationships from being registered to the metadata in the first place?"
    let query =
          T.unpack
            [st|
            query {
              #{albums} {
                artists_incompatible_columns {
                  #{name}
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "albums": [
                  {
                    "artists_incompatible_columns": null
                  },
                  {
                    "artists_incompatible_columns": null
                  },
                  {
                    "artists_incompatible_columns": null
                  },
                  {
                    "artists_incompatible_columns": null
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

-- | Tests for DB-to-DB joins behaviour of array relationships
--
-- Note: These array relationship tests also serve as schema generation tests
-- in that they test for the presence of certain fields.
remoteSourcesExecutionArrRelSpec :: SpecWith (SourceMetadata b, Application)
remoteSourcesExecutionArrRelSpec = describe "Array relationships" $ do
  it "Array relationships work with 'distinct_on'" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{albums}(distinct_on: #{artist_id}) {
                  #{artist_id}
                  #{title}
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "albums": [
                      {
                        "artist_id": 1,
                        "title": "Album 1A"
                      }
                    ]
                  },
                  {
                    "albums": [
                      {
                        "artist_id": 2,
                        "title": "Album 2A"
                      }
                    ]
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Array relationships work with 'distinct_on' (aggregate)" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{albums}_aggregate(distinct_on: #{artist_id}) {
                  nodes {
                    #{artist_id}
                    #{title}
                  }
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "albums_aggregate": {
                      "nodes": [
                        {
                          "artist_id": 1,
                          "title": "Album 1A"
                        }
                      ]
                    }
                  },
                  {
                    "albums_aggregate": {
                      "nodes": [
                        {
                          "artist_id": 2,
                          "title": "Album 2A"
                        }
                      ]
                    }
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Array relationships work with 'where'" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{albums}(where: {#{title}: {_like: "%1%"}}) {
                  #{title}
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "albums": [
                      {
                        "title": "Album 1A"
                      },
                      {
                        "title": "Album 1B"
                      }
                    ]
                  },
                  {
                    "albums": []
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Array relationships work with 'where' (aggregate)" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{albums}_aggregate(where: {#{title}: {_like: "%1%"}}) {
                  nodes {
                    #{title}
                  }
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "albums_aggregate": {
                      "nodes": [
                        {
                          "title": "Album 1A"
                        },
                        {
                          "title": "Album 1B"
                        }
                      ]
                    }
                  },
                  {
                    "albums_aggregate": {
                      "nodes": []
                    }
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Array relationships work with 'limit'" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{albums}(limit: 1) {
                  #{title}
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "albums": [
                      {
                        "title": "Album 1A"
                      }
                    ]
                  },
                  {
                    "albums": [
                      {
                        "title": "Album 2A"
                      }
                    ]
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Array relationships work with 'limit' (aggregate)" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{albums}_aggregate(limit: 1) {
                  nodes {
                    #{title}
                  }
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "albums_aggregate": {
                      "nodes": [
                        {
                          "title": "Album 1A"
                        }
                      ]
                    }
                  },
                  {
                    "albums_aggregate": {
                      "nodes": [
                        {
                          "title": "Album 2A"
                        }
                      ]
                    }
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Array relationships work with 'offset'" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{albums}(offset: 1) {
                  #{title}
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "albums": [
                      {
                        "title": "Album 1B"
                      }
                    ]
                  },
                  {
                    "albums": [
                      {
                        "title": "Album 2B"
                      }
                    ]
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Array relationships work with 'offset' (aggregate)" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{albums}_aggregate(offset: 1) {
                  nodes {
                    #{title}
                  }
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "albums_aggregate": {
                      "nodes": [
                        {
                          "title": "Album 1B"
                        }
                      ]
                    }
                  },
                  {
                    "albums_aggregate": {
                      "nodes": [
                        {
                          "title": "Album 2B"
                        }
                      ]
                    }
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Array relationships work with 'order_by'" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{albums}(order_by: {#{id_}: desc}) {
                  #{id_}
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "albums": [
                      {
                        "id": 2
                      },
                      {
                        "id": 1
                      }
                    ]
                  },
                  {
                    "albums": [
                      {
                        "id": 4
                      },
                      {
                        "id": 3
                      }
                    ]
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

  it "Array relationships work with 'order_by' (aggregate)" $ do
    let query =
          T.unpack
            [st|
            query {
              #{artists} {
                #{albums}_aggregate(order_by: {#{id_}: desc}) {
                  nodes {
                    #{id_}
                  }
                }
              }
            }
            |]
        expectedResponse =
          [json|
            {
              "data": {
                "artists": [
                  {
                    "albums_aggregate": {
                      "nodes": [
                        {
                          "id": 2
                        },
                        {
                          "id": 1
                        }
                      ]
                    }
                  },
                  {
                    "albums_aggregate": {
                      "nodes": [
                        {
                          "id": 4
                        },
                        {
                          "id": 3
                        }
                      ]
                    }
                  }
                ]
              }
            }
          |]
    v1graphql [] query `shouldRespondWith` expectedResponse

--------------------------------------------------------------------------------
-- Test setup

setup ::
  (Backend b) => RemoteSourceSql b -> SpecWith (SourceMetadata b, Application)
setup sqlConfig = do
  describe "Setup for DB-to-DB joins tests" $ do
    it "Setup initial metadata for runSQL" $ do
      initialSourceMetadata <- getState
      let metadata =
            emptyMetadata
              { _metaSources =
                  OM.fromList
                    [ ( _smName initialSourceMetadata,
                        mkAnyBackend initialSourceMetadata
                      )
                    ]
              }
          expectedResponse =
            "{\"is_consistent\":true,\"inconsistent_objects\":[]}"
      replaceMetadata metadata `shouldRespondWith` expectedResponse

    it "Populate database via runSQL" $ do
      let expectedResponse =
            "{\"result_type\":\"CommandOk\",\"result\":null}"
      -- TODO: Oops, this isn't backend-agnostic
      postgresRunSql (setupSql sqlConfig) `shouldRespondWith` expectedResponse

    it "Upload test metadata" $ do
      initialSourceMetadata <- getState
      let metadata =
            remoteSourceJoinsTestMetadata sqlConfig initialSourceMetadata
          expectedResponse =
            "{\"is_consistent\":true,\"inconsistent_objects\":[]}"
      replaceMetadata metadata `shouldRespondWith` expectedResponse

--------------------------------------------------------------------------------
-- Metadata

-- Currently defines two sources pointing to the same database
remoteSourceJoinsTestMetadata ::
  forall b. (Backend b) => RemoteSourceSql b -> SourceMetadata b -> Metadata
remoteSourceJoinsTestMetadata sqlConfig origSource =
  emptyMetadata
    { _metaSources =
        let connConfig = _smConfiguration origSource
            withName = _smName &&& (mkAnyBackend @b)
         in OM.fromList $
              map
                withName
                [ origSource,
                  albumsSource sqlConfig connConfig,
                  artistsSource sqlConfig connConfig
                ]
    }

--------------------------------------------------------------------------------
-- Albums source metadata

albumsSourceName :: SourceName
albumsSourceName = SNName $ mkNonEmptyTextUnsafe "albumsSource"

albumsSource ::
  Backend b =>
  RemoteSourceSql b ->
  SourceConnConfiguration b ->
  SourceMetadata b
albumsSource sqlConfig connConfig =
  SourceMetadata
    { _smName = albumsSourceName,
      _smTables =
        let withName = _tmTable &&& id
         in OM.fromList $ map withName [albumsTable sqlConfig],
      _smFunctions = mempty,
      _smConfiguration = connConfig,
      _smQueryTags = Nothing,
      _smCustomization = emptySourceCustomization
    }

albumsTable :: forall b. (Backend b) => RemoteSourceSql b -> TableMetadata b
albumsTable sqlConfig =
  baseTable
    { _tmRemoteRelationships =
        let withName = _rrName &&& id
         in OM.fromList $
              map
                withName
                [ albumsRemoteRel,
                  artistsRemoteRel,
                  artistsNullRemoteRel,
                  favouriteOfRemoteRel,
                  ambiguousArtistRemoteRel,
                  artistsIncompatibleColumnsRemoteRel
                ],
      _tmSelectPermissions =
        let withName = _pdRole &&& id
         in OM.fromList $
              map
                withName
                [ albumsObjRelUserSelectPermission,
                  albumsOnlyTitleSelectPermission,
                  albumsArrRelUserSelectPermission
                ]
    }
  where
    baseTable :: TableMetadata b
    baseTable = mkTableMeta (albumsTableName sqlConfig) False emptyTableConfig

    -- A self-relationship
    albumsRemoteRel :: RemoteRelationship
    albumsRemoteRel =
      RemoteRelationship
        { _rrName = RelName $ mkNonEmptyTextUnsafe albums,
          _rrDefinition =
            RelationshipToSource $
              ToSourceRelationshipDef
                { _tsrdRelationshipType = ObjRel,
                  _tsrdFieldMapping =
                    HM.singleton (FieldName id_) (FieldName id_),
                  _tsrdSource = albumsSourceName,
                  _tsrdTable = toJSON (albumsTableName sqlConfig)
                }
        }

    -- A basic object relationship
    artistsRemoteRel :: RemoteRelationship
    artistsRemoteRel =
      RemoteRelationship
        { _rrName = RelName $ mkNonEmptyTextUnsafe artists,
          _rrDefinition =
            RelationshipToSource $
              ToSourceRelationshipDef
                { _tsrdRelationshipType = ObjRel,
                  _tsrdFieldMapping =
                    HM.singleton (FieldName artist_id) (FieldName id_),
                  _tsrdSource = artistsSourceName,
                  _tsrdTable = toJSON (artistsTableName sqlConfig)
                }
        }

    -- An object relationship where some join columns are null
    artistsNullRemoteRel :: RemoteRelationship
    artistsNullRemoteRel =
      RemoteRelationship
        { _rrName =
            RelName $ mkNonEmptyTextUnsafe "artists_null",
          _rrDefinition =
            RelationshipToSource $
              ToSourceRelationshipDef
                { _tsrdRelationshipType = ObjRel,
                  _tsrdFieldMapping =
                    HM.fromList
                      [ (FieldName artist_id, FieldName id_),
                        (FieldName artist_id, FieldName id_null)
                      ],
                  _tsrdSource = artistsSourceName,
                  _tsrdTable = toJSON (artistsTableName sqlConfig)
                }
        }

    -- An object relationship that joins over two fields
    favouriteOfRemoteRel :: RemoteRelationship
    favouriteOfRemoteRel =
      RemoteRelationship
        { _rrName =
            RelName $ mkNonEmptyTextUnsafe "favouriteOf",
          _rrDefinition =
            RelationshipToSource $
              ToSourceRelationshipDef
                { _tsrdRelationshipType = ObjRel,
                  _tsrdFieldMapping =
                    HM.fromList
                      [ (FieldName artist_id, FieldName id_),
                        (FieldName title, FieldName fav_album)
                      ],
                  _tsrdSource = artistsSourceName,
                  _tsrdTable = toJSON (artistsTableName sqlConfig)
                }
        }

    -- An object relationship where some "source" rows are joined with more
    -- than one "target" row
    ambiguousArtistRemoteRel :: RemoteRelationship
    ambiguousArtistRemoteRel =
      RemoteRelationship
        { _rrName =
            RelName $ mkNonEmptyTextUnsafe "ambiguous_artist",
          _rrDefinition =
            RelationshipToSource $
              ToSourceRelationshipDef
                { _tsrdRelationshipType = ObjRel,
                  _tsrdFieldMapping =
                    HM.fromList
                      [ (FieldName artist_name, FieldName name)
                      ],
                  _tsrdSource = artistsSourceName,
                  _tsrdTable = toJSON (artistsTableName sqlConfig)
                }
        }

    -- A permissive select permission for role "objRel-user"
    albumsObjRelUserSelectPermission :: PermDef (SelPerm b)
    albumsObjRelUserSelectPermission =
      PermDef
        { _pdRole = fromJust $ mkRoleName "objRel-user",
          _pdPermission =
            SelPerm
              { spColumns = PCStar,
                spFilter = BoolExp gBoolExpTrue,
                spLimit = Nothing,
                spAllowAggregations = True,
                spComputedFields = []
              },
          _pdComment = Nothing
        }

    -- A permissive select permission for the role "only-albums". This role is
    -- intended to have full select permissions for the "albums" table but no
    -- select permissions for the "artists" table.
    albumsOnlyTitleSelectPermission :: PermDef (SelPerm b)
    albumsOnlyTitleSelectPermission =
      PermDef
        { _pdRole = fromJust $ mkRoleName "only-albums",
          _pdPermission =
            SelPerm
              { spColumns = PCStar,
                spFilter = BoolExp gBoolExpTrue,
                spLimit = Nothing,
                spAllowAggregations = True,
                spComputedFields = []
              },
          _pdComment = Nothing
        }

    -- An object relationship that attempts join incompatible columns. We
    -- expect a runtime error.
    artistsIncompatibleColumnsRemoteRel :: RemoteRelationship
    artistsIncompatibleColumnsRemoteRel =
      RemoteRelationship
        { _rrName =
            RelName $
              mkNonEmptyTextUnsafe "artists_incompatible_columns",
          _rrDefinition =
            RelationshipToSource $
              ToSourceRelationshipDef
                { _tsrdRelationshipType = ObjRel,
                  _tsrdFieldMapping =
                    HM.fromList
                      [ (FieldName id_, FieldName name)
                      ],
                  _tsrdSource = artistsSourceName,
                  _tsrdTable = toJSON (artistsTableName sqlConfig)
                }
        }

    -- A select permission for the role "arrRel-user" restricting access to (1)
    -- rows matching the user's ID and (2) the "artist_id" column, while also
    -- denying access to aggregations.
    albumsArrRelUserSelectPermission :: PermDef (SelPerm b)
    albumsArrRelUserSelectPermission =
      PermDef
        { _pdRole = fromJust $ mkRoleName "arrRel-user",
          _pdPermission =
            SelPerm
              { spColumns = PCCols [albumsArtistIdColumn sqlConfig],
                spFilter =
                  BoolExp $
                    BoolFld $
                      ColExp
                        { ceCol = FieldName artist_id,
                          ceVal =
                            Object $ HM.singleton "_eq" userIdHeader
                        },
                spLimit = Nothing,
                spAllowAggregations = False,
                spComputedFields = []
              },
          _pdComment = Nothing
        }

--------------------------------------------------------------------------------
-- Artists source metadata

artistsSourceName :: SourceName
artistsSourceName = SNName $ mkNonEmptyTextUnsafe "artistsSource"

artistsSource ::
  (Backend b) =>
  RemoteSourceSql b ->
  SourceConnConfiguration b ->
  SourceMetadata b
artistsSource sqlConfig connConfig =
  SourceMetadata
    { _smName = artistsSourceName,
      _smTables =
        let withName = _tmTable &&& id
         in OM.fromList $ map withName [artistsTable sqlConfig],
      _smFunctions = mempty,
      _smConfiguration = connConfig,
      _smQueryTags = Nothing,
      _smCustomization = emptySourceCustomization
    }

artistsTable :: forall b. (Backend b) => RemoteSourceSql b -> TableMetadata b
artistsTable sqlConfig =
  baseTable
    { _tmRemoteRelationships =
        let withName = _rrName &&& id
         in OM.fromList $
              map
                withName
                [ albumsRemoteRel,
                  albumsNullRemoteRel,
                  favAlbumsRemoteRel
                ],
      _tmSelectPermissions =
        let withName = _pdRole &&& id
         in OM.fromList $
              map
                withName
                [ artistsObjRelUserSelectPermission,
                  artistsArrRelUserSelectPermission,
                  artistsOnlyIdSelectPermission
                ]
    }
  where
    baseTable :: TableMetadata b
    baseTable = mkTableMeta (artistsTableName sqlConfig) False emptyTableConfig

    -- A basic array relationship
    albumsRemoteRel :: RemoteRelationship
    albumsRemoteRel =
      RemoteRelationship
        { _rrName = RelName $ mkNonEmptyTextUnsafe albums,
          _rrDefinition =
            RelationshipToSource $
              ToSourceRelationshipDef
                { _tsrdRelationshipType = ArrRel,
                  _tsrdFieldMapping =
                    HM.singleton (FieldName id_) (FieldName artist_id),
                  _tsrdSource = albumsSourceName,
                  _tsrdTable = toJSON (albumsTableName sqlConfig)
                }
        }

    -- An array relationship where some join columns are null
    albumsNullRemoteRel :: RemoteRelationship
    albumsNullRemoteRel =
      RemoteRelationship
        { _rrName =
            RelName $ mkNonEmptyTextUnsafe "albums_null",
          _rrDefinition =
            RelationshipToSource $
              ToSourceRelationshipDef
                { _tsrdRelationshipType = ArrRel,
                  _tsrdFieldMapping =
                    HM.fromList
                      [ (FieldName id_, FieldName artist_id),
                        (FieldName id_, FieldName artist_id_null)
                      ],
                  _tsrdSource = albumsSourceName,
                  _tsrdTable = toJSON (albumsTableName sqlConfig)
                }
        }

    -- An array relationship that joins over two fields
    favAlbumsRemoteRel :: RemoteRelationship
    favAlbumsRemoteRel =
      RemoteRelationship
        { _rrName = RelName $ mkNonEmptyTextUnsafe "favAlbums",
          _rrDefinition =
            RelationshipToSource $
              ToSourceRelationshipDef
                { _tsrdRelationshipType = ArrRel,
                  _tsrdFieldMapping =
                    HM.fromList
                      [ (FieldName id_, FieldName artist_id),
                        (FieldName fav_album, FieldName title)
                      ],
                  _tsrdSource = albumsSourceName,
                  _tsrdTable = toJSON (albumsTableName sqlConfig)
                }
        }

    -- A select permission for role "objRel-user" restricting access to (1)
    -- rows matching the user's ID and (2) the "id" column, while also denying
    -- access to aggregations.
    artistsObjRelUserSelectPermission :: PermDef (SelPerm b)
    artistsObjRelUserSelectPermission =
      PermDef
        { _pdRole = fromJust $ mkRoleName "objRel-user",
          _pdPermission =
            SelPerm
              { spColumns = PCCols [artistsIdColumn sqlConfig],
                spFilter =
                  BoolExp $
                    BoolFld $
                      ColExp
                        { ceCol = FieldName id_,
                          ceVal =
                            Object $ HM.singleton "_eq" userIdHeader
                        },
                spLimit = Nothing,
                spAllowAggregations = False,
                spComputedFields = []
              },
          _pdComment = Nothing
        }

    -- A permissive select permission for role "arrRel-user"
    artistsArrRelUserSelectPermission :: PermDef (SelPerm b)
    artistsArrRelUserSelectPermission =
      PermDef
        { _pdRole = fromJust $ mkRoleName "arrRel-user",
          _pdPermission =
            SelPerm
              { spColumns = PCStar,
                spFilter = BoolExp gBoolExpTrue,
                spLimit = Nothing,
                spAllowAggregations = True,
                spComputedFields = []
              },
          _pdComment = Nothing
        }

    -- A permissive select permission for the role "only-artists". This role is
    -- intended to have full select permissions for the "artists" table but no
    -- select permissions for the "albums" table.
    artistsOnlyIdSelectPermission :: PermDef (SelPerm b)
    artistsOnlyIdSelectPermission =
      PermDef
        { _pdRole = fromJust $ mkRoleName "only-artists",
          _pdPermission =
            SelPerm
              { spColumns = PCStar,
                spFilter = BoolExp gBoolExpTrue,
                spLimit = Nothing,
                spAllowAggregations = True,
                spComputedFields = []
              },
          _pdComment = Nothing
        }
