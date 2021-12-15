module Hasura.RQL.MetadataSpec
  ( spec,

    -- ** Test Helpers
    trippingJSON,
    trippingJSONValue,
    trippingJSONEncoding,
  )
where

-------------------------------------------------------------------------------

import Control.Lens ((%~), (^?!))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _Object)
import Data.HashMap.Strict qualified as HM
import Data.Yaml.TH (yamlQQ)
import GHC.Stack (HasCallStack)
import Hasura.Prelude hiding ((%~))
import Hasura.RQL.DDL.RemoteRelationship
  ( CreateFromSourceRelationship,
    LegacyCreateRemoteRelationship,
  )
import Hasura.RQL.Types.Metadata (Metadata)
import Hasura.SQL.Backend (BackendType (BigQuery, MSSQL, Postgres), PostgresKind (Vanilla))
import Hasura.Server.API.Metadata (RQLMetadataV1)
import Hasura.Server.API.Query qualified as V1 (RQLQuery)
import Hedgehog (MonadTest, evalEither, tripping)
import Test.Hspec (Spec, describe, expectationFailure, it)
import Test.Hspec.Hedgehog (hedgehog)

-------------------------------------------------------------------------------

spec :: Spec
spec = describe "Remote Relationship Metadata" do
  spec_roundtrip
  spec_Metadata_examples
  spec_RQLQuery_examples
  spec_RQLMetadataV1_examples

-------------------------------------------------------------------------------

spec_roundtrip :: Spec
spec_roundtrip = describe "Roundtrip" do
  describe "Metadata" do
    it "passes JSON roundtrip tests for an example remote relationship fragment" $
      hedgehog do
        metadata :: Metadata <-
          evalAesonResult $
            Aeson.fromJSON remote_relationship_metadata_fragment
        trippingJSONValue metadata

  describe "CreateFromSourceRelationship" do
    it "passes JSON roundtrip tests for a 'pg_create_remote_relationship' query fragment" $
      hedgehog $ do
        let fragment = pg_create_remote_relationship_fragment ^?! key "args"
        cfsr :: (CreateFromSourceRelationship ('Postgres 'Vanilla)) <-
          evalAesonResult $ Aeson.fromJSON fragment
        trippingJSON cfsr

    it "passes JSON roundtrip tests for an 'mssql_create_remote_relationship' query fragment" $
      hedgehog $ do
        let fragment = mssql_create_remote_relationship_fragment ^?! key "args"
        cfsr :: (CreateFromSourceRelationship 'MSSQL) <-
          evalAesonResult $ Aeson.fromJSON fragment
        trippingJSON cfsr

    it "passes JSON roundtrip tests for a 'bigquery_create_remote_relationship' query fragment" $
      hedgehog $ do
        let fragment = bigquery_create_remote_relationship_fragment ^?! key "args"
        cfsr :: (CreateFromSourceRelationship 'BigQuery) <-
          evalAesonResult $ Aeson.fromJSON fragment
        trippingJSON cfsr

  describe "LegacyCreateRemoteRelationship" do
    it "passes JSON roundtrip tests for a 'create_remote_relationship' query fragment" $
      hedgehog do
        let fragment = create_remote_relationship_fragment ^?! key "args"
        lcrr :: LegacyCreateRemoteRelationship <-
          evalAesonResult $ Aeson.fromJSON fragment
        trippingJSON lcrr

-------------------------------------------------------------------------------

spec_Metadata_examples :: Spec
spec_Metadata_examples = describe "Metadata" $ do
  it "parses an example remote relationship metadata fragment" do
    case Aeson.fromJSON @Metadata remote_relationship_metadata_fragment of
      Aeson.Success _ -> pure ()
      Aeson.Error err -> expectationFailure err

-------------------------------------------------------------------------------

spec_RQLQuery_examples :: Spec
spec_RQLQuery_examples = describe "V1 RQLQuery" do
  it "parses a 'create_remote_relationship' query fragment as a V1 'RQLQuery' type" do
    case Aeson.fromJSON @V1.RQLQuery create_remote_relationship_fragment of
      Aeson.Success _ -> pure ()
      Aeson.Error err -> expectationFailure err

-------------------------------------------------------------------------------

spec_RQLMetadataV1_examples :: Spec
spec_RQLMetadataV1_examples = describe "RQLMetadataV1" do
  it "parses a 'create_remote_relationship' query fragment" do
    case Aeson.fromJSON @RQLMetadataV1 create_remote_relationship_fragment of
      Aeson.Success _ -> pure ()
      Aeson.Error err -> expectationFailure err

  it "parses a 'pg_create_remote_relationship' query fragment" do
    case Aeson.fromJSON @RQLMetadataV1 pg_create_remote_relationship_fragment of
      Aeson.Success _ -> pure ()
      Aeson.Error err -> expectationFailure err

  it "parses a 'bigquery_create_remote_relationship' query fragment" do
    case Aeson.fromJSON @RQLMetadataV1 bigquery_create_remote_relationship_fragment of
      Aeson.Success _ -> pure ()
      Aeson.Error err -> expectationFailure err

-------------------------------------------------------------------------------
-- Example YAML fragments for the metadata and query tests above.

remote_relationship_metadata_fragment :: Aeson.Value
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

create_remote_relationship_fragment :: Aeson.Value
create_remote_relationship_fragment =
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

-- | Backend-agnostic query fragment which omits the @type@ field.
--
-- This should be used to construct backend-specific fragments by adding the
-- correct type and/or modifying any of the fields specified here as needed.
--
-- See 'pg_create_remote_relationship_fragment' for details.
backend_create_remote_relationship_fragment :: Aeson.Value
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

pg_create_remote_relationship_fragment :: Aeson.Value
pg_create_remote_relationship_fragment =
  backend_create_remote_relationship_fragment
    & _Object %~ HM.insert ("type" :: Text) "pg_create_remote_relationship"

mssql_create_remote_relationship_fragment :: Aeson.Value
mssql_create_remote_relationship_fragment =
  backend_create_remote_relationship_fragment
    & _Object %~ HM.insert ("type" :: Text) "mssql_create_remote_relationship"

-- NOTE: The 'BigQuery' backend expects its @table@ argument to be of type
-- 'Aeson.Object' (all of the other backends support 'Aeson.String').
--
-- Rather than trying to wrangle even more of this with @lens-aeson@, it's
-- easier to just duplicate the structure in-place for the time being.
bigquery_create_remote_relationship_fragment :: Aeson.Value
bigquery_create_remote_relationship_fragment =
  [yamlQQ|
type: bigquery_create_remote_relationship
args:
  name: message
  table:
    name: profiles
    dataset: test
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

-------------------------------------------------------------------------------
-- Utility functions.
--
-- NOTE(jkachmar): These are probably generally useful, and should be moved out
-- to some sort of test prelude.

-- | Fails the test if the 'Aeson.Result' is 'Aeson.Error', otherwise returns
-- the value in 'Aeson.Success'.
evalAesonResult ::
  forall m a.
  (MonadTest m, HasCallStack) =>
  Aeson.Result a ->
  m a
evalAesonResult x = evalEither $ case x of
  Aeson.Success val -> Right val
  Aeson.Error err -> Left err

-- | Test that the 'Aeson.toJSON' / 'Aeson.fromJSON' and 'Aeson.encode' /
-- 'Aeson.decode' functions are compatible with one another (respectively).
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

-- | Test that 'Aeson.toJSON' / 'Aeson.fromJSON' functions are compatible for a
-- given value.
--
-- This verifies that the 'parseJSON' and 'toJSON' instances agree with one
-- another.
trippingJSONValue ::
  forall a m.
  (FromJSON a, ToJSON a, Eq a, Show a, MonadTest m) =>
  a ->
  m ()
trippingJSONValue x = tripping x Aeson.toJSON Aeson.fromJSON

-- | Test that 'Aeson.encode' / 'Aeson.decode' functions are compatible for a
-- given value.
--
-- This verifies that the 'parseJSON' and 'toEncoding' instances agree with one
-- another.
trippingJSONEncoding ::
  forall a m.
  (FromJSON a, ToJSON a, Eq a, Show a, MonadTest m) =>
  a ->
  m ()
trippingJSONEncoding x = tripping x Aeson.encode Aeson.eitherDecode'
