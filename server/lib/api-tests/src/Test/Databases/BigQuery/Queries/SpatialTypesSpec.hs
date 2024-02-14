{-# LANGUAGE QuasiQuotes #-}

-- | Test support for geospatial data types on BigQuery
-- https://cloud.google.com/bigquery/docs/geospatial-data
module Test.Databases.BigQuery.Queries.SpatialTypesSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.BigQuery qualified as BigQuery
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..))
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend BigQuery.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ BigQuery.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "spatial_types_geography")
      { Schema.tableColumns =
          [ Schema.column "point" Schema.TGeography,
            Schema.column "linestring" Schema.TGeography,
            Schema.column "polygon" Schema.TGeography,
            Schema.column "multipoint" Schema.TGeography,
            Schema.column "multilinestring" Schema.TGeography,
            Schema.column "multipolygon" Schema.TGeography,
            Schema.column "geometrycollection" Schema.TGeography
          ],
        Schema.tablePrimaryKey = ["id"],
        Schema.tableData =
          [ [ Schema.VGeography "POINT(3 4)",
              Schema.VGeography "LINESTRING(1 1, 2 3, 4 8, -6 3)",
              Schema.VGeography "POLYGON((2 1, 1 2, 1 1, 2 1))",
              Schema.VGeography "MULTIPOINT(2 3, 7 8)",
              Schema.VGeography "LINESTRING(1 1, 3 3, 5 5, 7 7)",
              Schema.VGeography "MULTIPOLYGON(((-120.533 46.566, -118.283 46.1, -122.3 47.45, -120.533 46.566)), ((-2 2, -2 -2, 2 -2, 2 2, -2 2)))",
              Schema.VGeography "GEOMETRYCOLLECTION(LINESTRING(1 1, 3 5), POLYGON((-5 -1, -5 -5, -1 -5, -1 -1, -5 -1)))"
            ]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Geography scalar type" do
    it "Fetch geospatial data" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
      let expected :: Value
          expected =
            [interpolateYaml|
            data:
              #{schemaName}_spatial_types_geography:
                - point: POINT(3 4)
                  linestring: LINESTRING(1 1, 2 3, 4 8, -6 3)
                  polygon: POLYGON((2 1, 1 2, 1 1, 2 1))
                  multipoint: MULTIPOINT(2 3, 7 8)
                  multilinestring:
                    LINESTRING(1 1, 3 3, 5 5, 7 7)
                  multipolygon:
                    MULTIPOLYGON(((-120.533 46.566, -118.283 46.1, -122.3 47.45, -120.533 46.566)), ((-2 2, -2 -2, 2 -2, 2 2, -2 2)))
                  geometrycollection:
                    GEOMETRYCOLLECTION(LINESTRING(1 1, 3 5), POLYGON((-5 -1, -5 -5, -1 -5, -1 -1, -5 -1)))
          |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_spatial_types_geography {
                    point
                    linestring
                    polygon
                    multipoint
                    multilinestring
                    multipolygon
                    geometrycollection
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Fetch geospatial data filtered on _st_equals" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
      let expected :: Value
          expected =
            [interpolateYaml|
            data:
              #{schemaName}_spatial_types_geography:
                - geometrycollection:
                    GEOMETRYCOLLECTION(LINESTRING(1 1, 3 5), POLYGON((-5 -1, -5 -5, -1 -5, -1 -1, -5 -1)))
          |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_spatial_types_geography(
                    where: {
                      geometrycollection: {
                        _st_equals: "GEOMETRYCOLLECTION(LINESTRING(1 1, 3 5), POLYGON((-5 -1, -5 -5, -1 -5, -1 -1, -5 -1)))"
                      }
                    }
                  ) {
                    geometrycollection
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Fetch geospatial data filtered on _st_contains" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
      let expected :: Value
          expected =
            [interpolateYaml|
            data:
              #{schemaName}_spatial_types_geography:
                - multipolygon:
                    MULTIPOLYGON(((-120.533 46.566, -118.283 46.1, -122.3 47.45, -120.533 46.566)), ((-2 2, -2 -2, 2 -2, 2 2, -2 2)))
          |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_spatial_types_geography(
                    where: {
                      multipolygon: {
                        _st_contains: "POINT(0.5 0)"
                      }
                    }
                  ) {
                    multipolygon
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Fetch geospatial data filtered on _st_intersects" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
      let expected :: Value
          expected =
            [interpolateYaml|
            data:
              #{schemaName}_spatial_types_geography:
                - polygon: POLYGON((2 1, 1 2, 1 1, 2 1))
          |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_spatial_types_geography(
                    where: {
                      polygon: {
                        _st_intersects: "LINESTRING(0 0, 2 2)"
                      }
                    }
                  ) {
                    polygon
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Fetch geospatial data filtered on _st_within" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
      let expected :: Value
          expected =
            [interpolateYaml|
            data:
              #{schemaName}_spatial_types_geography:
                - multipoint: MULTIPOINT(2 3, 7 8)
          |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_spatial_types_geography(
                    where: {
                      multipoint: {
                        _st_within: "POLYGON ((0 0, 10 10, 10 10, 10 0, 0 0))"
                      }
                    }
                  ) {
                    multipoint
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Fetch geospatial data filtered on _st_d_within" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
      let expected :: Value
          expected =
            [interpolateYaml|
            data:
              #{schemaName}_spatial_types_geography:
                - multipoint: MULTIPOINT(2 3, 7 8)
          |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_spatial_types_geography(
                    where: {
                      multipoint: {
                        _st_d_within: { distance: 1, from: "POLYGON ((0 0, 10 10, 10 10, 10 0, 0 0))"}
                      }
                    }
                  ) {
                    multipoint
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Fetch geospatial data filtered on _st_touches" \testEnvironment -> do
      let schemaName = Schema.getSchemaName testEnvironment
      let expected :: Value
          expected =
            [interpolateYaml|
            data:
              #{schemaName}_spatial_types_geography:
                - point: POINT(3 4)
          |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                query {
                  #{schemaName}_spatial_types_geography(
                    where: {
                      point: {
                        _st_touches: "POLYGON ((3 4, 2 5, 5 5, 5 2, 3 4))"
                      }
                    }
                  ) {
                    point
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected
