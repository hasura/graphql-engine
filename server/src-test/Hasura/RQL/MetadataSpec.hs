{-# LANGUAGE QuasiQuotes #-}

module Hasura.RQL.MetadataSpec
  ( spec,

    -- ** Test Helpers
    trippingJSON,
    trippingJSONValue,
    trippingJSONEncoding,
  )
where

-------------------------------------------------------------------------------

import Control.Lens ((%~), (.~), (^?!))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens (key, _Object)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Typeable (Typeable, typeRep)
import Data.Yaml.TH (yamlQQ)
import GHC.Stack (HasCallStack)
import Hasura.Prelude hiding ((%~))
import Hasura.RQL.DDL.RemoteRelationship
  ( CreateFromSourceRelationship,
  )
import Hasura.RQL.Types.BackendType (BackendType (BigQuery, MSSQL, Postgres), PostgresKind (Vanilla))
import Hasura.RQL.Types.Metadata (Metadata)
import Hasura.Server.API.Metadata (RQLMetadataV1)
import Hasura.Server.API.Query qualified as V1 (RQLQuery)
import Hedgehog (MonadTest, evalEither, tripping)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldContain)
import Test.Hspec.Hedgehog (hedgehog)

-------------------------------------------------------------------------------

spec :: Spec
spec = describe "Remote Relationship Metadata" do
  spec_roundtrip
  spec_Metadata_examples
  spec_RQLQuery_examples
  spec_RQLMetadataV1_examples
  spec_query_tags_examples

-------------------------------------------------------------------------------

spec_roundtrip :: Spec
spec_roundtrip = describe "JSON Roundtrip" do
  describe "Metadata" do
    it "example remote relationship fragment"
      $ hedgehog do
        metadata :: Metadata <-
          evalAesonResult
            $ J.fromJSON remote_relationship_metadata_fragment
        trippingJSONValue metadata

  describe "CreateFromSourceRelationship" do
    it "'pg_create_remote_relationship' query"
      $ hedgehog
      $ do
        let argument = mk_pg_remote_relationship_argument "create" ^?! key "args"
        cfsr :: (CreateFromSourceRelationship ('Postgres 'Vanilla)) <-
          evalAesonResult $ J.fromJSON argument
        trippingJSON cfsr

    it "'pg_create_remote_relationship' query with the 'old' schema"
      $ hedgehog
      $ do
        let argument = mk_pg_remote_relationship_old_argument "create" ^?! key "args"
        cfsr :: (CreateFromSourceRelationship ('Postgres 'Vanilla)) <-
          evalAesonResult $ J.fromJSON argument
        trippingJSON cfsr

    it "'mssql_create_remote_relationship' query"
      $ hedgehog
      $ do
        let argument = mk_mssql_remote_relationship_argument "create" ^?! key "args"
        cfsr :: (CreateFromSourceRelationship 'MSSQL) <-
          evalAesonResult $ J.fromJSON argument
        trippingJSON cfsr

    it "'bigquery_create_remote_relationship' query"
      $ hedgehog
      $ do
        let argument = mk_bigquery_remote_relationship_argument "create" ^?! key "args"
        cfsr :: (CreateFromSourceRelationship 'BigQuery) <-
          evalAesonResult $ J.fromJSON argument
        trippingJSON cfsr

-------------------------------------------------------------------------------

spec_Metadata_examples :: Spec
spec_Metadata_examples = describe "Metadata" $ do
  it "parses an example remote relationship metadata fragment" do
    decodesJSON @Metadata remote_relationship_metadata_fragment

-------------------------------------------------------------------------------

spec_RQLQuery_examples :: Spec
spec_RQLQuery_examples = describe "V1 RQLQuery" do
  it "parses a 'create_remote_relationship' query with the 'new' schema" do
    decodesJSON @V1.RQLQuery
      [yamlQQ|
      type: create_remote_relationship
      args:
        name: message
        table: profiles
        definition:
          to_remote_schema:
            lhs_fields:
              - id
              - name
            remote_schema: my-remote-schema
            remote_field:
              message:
                arguments:
                  id: "$id"
    |]

  it "parses a 'create_remote_relationship' query with the 'old' schema" do
    decodesJSON @V1.RQLQuery
      [yamlQQ|
     type: create_remote_relationship
     args:
       name: message
       table: profiles
       hasura_fields:
         - id
         - name
       remote_schema: my-remote-schema
       remote_field:
         message:
           arguments:
             id: "$id"
   |]

  it "rejects a 'create_remote_relationship' query with the combination 'new+old' schema" do
    rejectsJSON @V1.RQLQuery
      "expects exactly one of: to_source, to_remote_schema"
      [yamlQQ|
      type: create_remote_relationship
      args:
        name: message
        table: profiles
        # Note remote_schema nested under definition!
        definition:
          hasura_fields:
            - id
            - name
          remote_schema: my-remote-schema
          remote_field:
            message:
              arguments:
                id: "$id"
    |]

-------------------------------------------------------------------------------

spec_RQLMetadataV1_examples :: Spec
spec_RQLMetadataV1_examples = describe "RQLMetadataV1" do
  describe "Success" do
    for_ ["create", "update", "delete"] \action ->
      it ("parses a 'pg_" <> T.unpack action <> "_remote_relationship query") do
        decodesJSON @RQLMetadataV1 $ mk_pg_remote_relationship_argument action

    for_ ["create", "update", "delete"] \action ->
      it ("parses a 'pg_" <> T.unpack action <> "_remote_relationship query using the 'old' schema") do
        decodesJSON @RQLMetadataV1 $ mk_pg_remote_relationship_old_argument action

    for_ ["create", "update", "delete"] \action ->
      it ("parses a 'citus_" <> T.unpack action <> "_remote_relationship query") do
        decodesJSON @RQLMetadataV1 $ mk_citus_remote_relationship_argument action

    for_ ["create", "update", "delete"] \action ->
      it ("parses a 'bigquery_" <> T.unpack action <> "_remote_relationship query") do
        decodesJSON @RQLMetadataV1 $ mk_bigquery_remote_relationship_argument action

    for_ ["create", "update", "delete"] \action ->
      it ("parses a 'mssql_" <> T.unpack action <> "_remote_relationship query") do
        decodesJSON @RQLMetadataV1 $ mk_mssql_remote_relationship_argument action

  describe "Failure" do
    for_ ["create", "update"] \action ->
      it ("fails to parse a 'pg_" <> T.unpack action <> "_remote_relationship query using the 'old+new' schema") do
        rejectsJSON @RQLMetadataV1
          "expects exactly one of: to_source, to_remote_schema"
          $ mk_pg_remote_relationship_old_new_argument action

-------------------------------------------------------------------------------

-- |
-- > type: 'set_query_tags'
-- > args:
-- >   source_name: # Name of the source | Required
-- >   disabled: # Optional Field | Type: Bool | Values: true or false
-- >   format: # Optional Field  | Values: standard or sqlcommenter
-- >   omit_request_id: # Optional Field | Type: Bool | Values: true or false
spec_query_tags_examples :: Spec
spec_query_tags_examples =
  describe "RQLMetadataV1 set_query_tags" do
    for_
      [ (disabled, format, omit_request_id)
        | disabled <- Nothing : map Just (enumerate @Bool),
          format <- Nothing : map Just ["standard", "sqlcommenter"],
          omit_request_id <- Nothing : map Just (enumerate @Bool)
      ]
      \(mdisabled, mformat, momit_request_id) ->
        it ("decodes with (disabled, format, omit_request_id) set to " <> show (mdisabled, mformat, momit_request_id)) do
          decodesJSON @RQLMetadataV1
            $ J.object
              [ "type" J..= J.String "set_query_tags",
                "args"
                  J..= J.object
                    ( ["source_name" J..= J.String "postgres"]
                        <> ["disabled" J..= J.Bool disabled | Just disabled <- [mdisabled]]
                        <> ["format" J..= J.String format | Just format <- [mformat]]
                        <> ["omit_request_id" J..= J.Bool omit_request_id | Just omit_request_id <- [momit_request_id]]
                    )
              ]

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

-------------------------------------------------------------------------------
-- Example YAML fragments for the metadata and query tests above.

remote_relationship_metadata_fragment :: J.Value
remote_relationship_metadata_fragment =
  [yamlQQ|
version: 3
sources:
- name: something
  kind: postgres
  configuration:
    connection_info:
      database_url: something
  tables:
  - table: test
    remote_relationships:
    - name: remote
      definition:
        hasura_fields: [id]
        remote_field:
          some_fiend_name:
            arguments:
              id: $id
        remote_schema: some_remote_schema_name
    |]

-- | Backend-agnostic @v1/metadata@ argument fragment which omits the @type@
-- field.
--
-- This should be used to construct backend-specific fragments by adding the
-- correct type and/or modifying any of the fields specified here as needed.
--
-- See 'mk_backend_remote_relationship_argument for example usage.
backend_create_remote_relationship_fragment :: J.Value
backend_create_remote_relationship_fragment =
  [yamlQQ|
args:
  name: message
  table: profiles
  definition:
    to_remote_schema:
      lhs_fields:
        - id
        - name
      remote_schema: my-remote-schema
      remote_field:
        message:
          arguments:
            id: "$id"
  |]

-- | Constructor for @v1/metadata@ @<backend>_(create|update|delete)_remote_relationship@
-- arguments using the new, unified schema.
--
-- See 'mk_pg_backend_remote_relationship_argument for example usage.
mk_backend_remote_relationship_argument :: Text -> Text -> J.Value
mk_backend_remote_relationship_argument backend action =
  backend_create_remote_relationship_fragment
    & _Object
    %~ KM.insert
      (Key.fromText "type")
      (J.String $ backend <> "_" <> action <> "_remote_relationship")

-- | Constructor for @v1/metadata@ @mssql_(create|update|delete)_remote_relationship@
-- arguments using the new, unified schema.
mk_mssql_remote_relationship_argument :: Text -> J.Value
mk_mssql_remote_relationship_argument action =
  mk_backend_remote_relationship_argument "mssql" action

-- | Constructor for @v1/metadata@ @citus_(create|update|delete)_remote_relationship@
-- arguments using the new, unified schema.
mk_citus_remote_relationship_argument :: Text -> J.Value
mk_citus_remote_relationship_argument action =
  mk_backend_remote_relationship_argument "citus" action

-- | Constructor for @v1/metadata@ @pg_(create|update|delete)_remote_relationship@
-- arguments using the new, unified schema.
mk_pg_remote_relationship_argument :: Text -> J.Value
mk_pg_remote_relationship_argument action =
  mk_backend_remote_relationship_argument "pg" action

-- | Constructor for @v1/metadata@ @bigquery_(create|update|delete)_remote_relationship@
-- arguments using the new, unified schema.
--
-- NOTE: The 'BigQuery' backend expects its @table@ argument to be of type
-- 'J.Object' (all of the other backends support 'J.String').
mk_bigquery_remote_relationship_argument :: Text -> J.Value
mk_bigquery_remote_relationship_argument action =
  mk_backend_remote_relationship_argument "bigquery" action
    & key "args"
    . key "table"
    .~ J.Object
      ( KM.fromList
          [ ("name", "profiles"),
            ("dataset", "test")
          ]
      )

-- | Constructor for @v1/metadata@ @pg_(create|update|delete)_remote_relationship@
-- arguments using the old, non-unified schema.
mk_pg_remote_relationship_old_argument :: Text -> J.Value
mk_pg_remote_relationship_old_argument action =
  fragment
    & _Object
    %~ KM.insert
      (Key.fromText "type")
      (J.String $ "pg_" <> action <> "_remote_relationship")
  where
    fragment =
      [yamlQQ|
args:
  name: message
  table: profiles
  hasura_fields:
    - id
    - name
  remote_schema: my-remote-schema
  remote_field:
    message:
      arguments:
        id: "$id"
|]

-- | Constructor for @v1/metadata@ @pg_(create|update|delete)_remote_relationship@
-- arguments using a mix of the old and new schema.
mk_pg_remote_relationship_old_new_argument :: Text -> J.Value
mk_pg_remote_relationship_old_new_argument action =
  fragment
    & _Object
    %~ KM.insert
      (Key.fromText "type")
      (J.String $ "pg_" <> action <> "_remote_relationship")
  where
    fragment =
      [yamlQQ|
args:
  name: message
  table: profiles
  definition:
    hasura_fields:
      - id
      - name
    remote_schema: my-remote-schema
    remote_field:
      message:
        arguments:
          id: "$id"
|]

-------------------------------------------------------------------------------
-- Utility functions.
--
-- NOTE(jkachmar): These are probably generally useful, and should be moved out
-- to some sort of test prelude.

-- | Fails the test if the 'J.Result' is 'J.Error', otherwise returns
-- the value in 'J.Success'.
evalAesonResult ::
  forall m a.
  (MonadTest m, HasCallStack) =>
  J.Result a ->
  m a
evalAesonResult x = evalEither $ case x of
  J.Success val -> Right val
  J.Error err -> Left err

-- | Test that the 'J.toJSON' / 'J.fromJSON' and 'J.encode' /
-- 'J.decode' functions are compatible with one another (respectively).
--
-- This is principally useful for validating manually implemented 'toEncoding'
-- methods (typically used to improve serialization performance).
trippingJSON ::
  forall a m.
  (FromJSON a, ToJSON a, Eq a, Show a, MonadTest m) =>
  a ->
  m ()
trippingJSON x = do
  trippingJSONValue x
  trippingJSONEncoding x

-- | Test that 'J.toJSON' / 'J.fromJSON' functions are compatible for a
-- given value.
--
-- This verifies that the 'parseJSON' and 'toJSON' instances agree with one
-- another.
trippingJSONValue ::
  forall a m.
  (FromJSON a, ToJSON a, Eq a, Show a, MonadTest m) =>
  a ->
  m ()
trippingJSONValue x = tripping x J.toJSON J.fromJSON

-- | Test that 'J.encode' / 'J.decode' functions are compatible for a
-- given value.
--
-- This verifies that the 'parseJSON' and 'toEncoding' instances agree with one
-- another.
trippingJSONEncoding ::
  forall a m.
  (FromJSON a, ToJSON a, Eq a, Show a, MonadTest m) =>
  a ->
  m ()
trippingJSONEncoding x = tripping x J.encode J.eitherDecode'

-- | Test that a specific JSON value can be parsed successfully.
decodesJSON :: forall a. (HasCallStack, FromJSON a) => J.Value -> IO ()
decodesJSON value = case J.fromJSON @a value of
  J.Success _ -> pure ()
  J.Error err -> expectationFailure err

-- | Test that a specific JSON value fails to parse.
rejectsJSON :: forall a. (HasCallStack, Typeable a, FromJSON a) => String -> J.Value -> IO ()
rejectsJSON message value = case J.fromJSON @a value of
  J.Error err -> err `shouldContain` message
  J.Success _ ->
    expectationFailure
      $ mconcat
        [ "expected parsing ",
          show $ typeRep $ Proxy @a,
          " to fail, but it succeeded"
        ]
