module Hasura.RQL.DDL.CustomTypesSpec (spec) where

import Data.Aeson qualified as J
import Data.Either (isLeft, isRight)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.DDL.CustomTypes
import Hasura.RQL.Types.Backend (parseScalarValue)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.NamingCase
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.BackendMap qualified as BackendMap
import Language.GraphQL.Draft.Syntax qualified as G
import Test.Hspec

spec :: Spec
spec = describe "Postgres scalar aliases in action custom types" do
  for_ [HasuraCase, GraphqlCase] \discoveredCase ->
    for_ [HasuraCase, GraphqlCase] \requestedCase ->
      for_ [PGJSONB, PGBigInt, PGTimeStamp, PGTimeStampTZ] \scalar ->
        it ("resolves " <> show scalar <> " from " <> show discoveredCase <> " using " <> show requestedCase) do
          let discoveredName = scalarName discoveredCase scalar
              requestedName = scalarName requestedCase scalar
              wrapped = ScalarWrapper scalar () :: ScalarWrapper ('Postgres 'Vanilla)
              scalars = BackendMap.singleton $ ScalarParsingMap $ HashMap.singleton discoveredName wrapped
          case lookupBackendScalarWithAliases scalars requestedName of
            Just (ASTReusedScalar name resolved) -> do
              name `shouldBe` requestedName
              (AB.unpackAnyBackend @('Postgres 'Vanilla) resolved == Just wrapped) `shouldBe` True
            _ -> expectationFailure "expected the original backend scalar and requested GraphQL spelling"

  it "does not make scalar lookup generally case-insensitive" do
    let scalars = pgScalars [("jsonb", PGJSONB)]
    (lookupBackendScalarWithAliases scalars (gName "JSONB") == Nothing) `shouldBe` True

  it "does not make the exact lookup accept aliases" do
    let scalars = pgScalars [("jsonb", PGJSONB)]
    (lookupBackendScalar scalars (gName "Jsonb") == Nothing) `shouldBe` True

  it "accepts convention-generated names for extension scalars" do
    let scalar = PGUnknown "some_extension_type"
        scalars = pgScalars [("some_extension_type", scalar)]
    case lookupBackendScalarWithAliases scalars (scalarName GraphqlCase scalar) of
      Just (ASTReusedScalar _ resolved) ->
        (AB.unpackAnyBackend @('Postgres 'Vanilla) resolved == Just (ScalarWrapper scalar ())) `shouldBe` True
      _ -> expectationFailure "expected a generated extension-scalar alias"

  it "deduplicates aliases of the same backend scalar" do
    let scalars = pgScalars [("first_entry", PGJSONB), ("second_entry", PGJSONB)]
    case lookupBackendScalarWithAliases scalars (gName "Jsonb") of
      Just (ASTReusedScalar _ resolved) ->
        (AB.unpackAnyBackend @('Postgres 'Vanilla) resolved == Just (ScalarWrapper PGJSONB ())) `shouldBe` True
      _ -> expectationFailure "expected one distinct backend scalar"

  it "does not pick an arbitrary parser for an ambiguous new alias" do
    let scalars =
          pgScalars
            [ ("some_type", PGUnknown "some_type"),
              ("someType", PGUnknown "someType")
            ]
    scalarName GraphqlCase (PGUnknown "some_type") `shouldBe` scalarName GraphqlCase (PGUnknown "someType")
    (lookupBackendScalarWithAliases scalars (gName "SomeType") == Nothing) `shouldBe` True

  it "preserves an exact name even when another scalar generates that alias" do
    let scalars =
          pgScalars
            [ ("jsonb", PGJSONB),
              ("Jsonb", PGUnknown "Jsonb")
            ]
    (lookupBackendScalarWithAliases scalars (gName "Jsonb") == lookupBackendScalar scalars (gName "Jsonb")) `shouldBe` True

  it "rejects ambiguous new aliases across Postgres backends" do
    let vanilla = pgScalars [("jsonb", PGJSONB)]
        citus =
          BackendMap.singleton @('Postgres 'Citus)
            $ ScalarParsingMap
            $ HashMap.singleton (gName "jsonb") (ScalarWrapper PGJSONB ())
    (lookupBackendScalarWithAliases (vanilla <> citus) (gName "Jsonb") == Nothing) `shouldBe` True

  it "retains Postgres input validation when resolving an alias" do
    let scalars = pgScalars [("Bigint", PGBigInt)]
    case lookupBackendScalarWithAliases scalars (gName "bigint") of
      Just (ASTReusedScalar _ resolved) ->
        case AB.unpackAnyBackend @('Postgres 'Vanilla) resolved of
          Just (ScalarWrapper scalar parsingContext) -> do
            (isRight $ parseScalarValue @('Postgres 'Vanilla) parsingContext scalar (J.Number 42)) `shouldBe` True
            (isLeft $ parseScalarValue @('Postgres 'Vanilla) parsingContext scalar (J.Bool True)) `shouldBe` True
          Nothing -> expectationFailure "expected Postgres parsing context"
      _ -> expectationFailure "expected reused scalar, not a generic JSON custom scalar"

scalarName :: NamingCase -> PGScalarType -> G.Name
scalarName namingCase scalar =
  either (error . T.unpack . showQErr) id (mkScalarTypeName namingCase scalar :: Either QErr G.Name)

gName :: Text -> G.Name
gName name = fromMaybe (error "invalid test GraphQL name") $ G.mkName name

pgScalars :: [(Text, PGScalarType)] -> BackendMap.BackendMap ScalarParsingMap
pgScalars entries =
  BackendMap.singleton @('Postgres 'Vanilla)
    $ ScalarParsingMap
    $ HashMap.fromList
    $ map (\(name, scalar) -> (gName name, ScalarWrapper scalar ())) entries
