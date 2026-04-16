module Hasura.GraphQL.Logging.QueryLogSpec
  ( spec,
  )
where

import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasura.GraphQL.Logging.QueryLog
import Hasura.GraphQL.Namespace (RootFieldAlias (..))
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as Protocol
import Hasura.Prelude
import Hasura.Server.Types (RequestId (..))
import Language.GraphQL.Draft.Syntax qualified as G
import Test.Hspec qualified as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "QueryLog Masking" $ do
  maskQueryLogSpec
  maskJsonValueSpec

mkName :: Text -> G.Name
mkName t = fromMaybe (error "invalid name") (G.mkName t)

mkVariableValues :: [(Text, J.Value)] -> Protocol.VariableValues
mkVariableValues = HashMap.fromList . map (first mkName)

mkTestQueryLog :: Protocol.VariableValues -> Maybe GeneratedQuery -> QueryLog
mkTestQueryLog vars mGenSql =
  QueryLog
    { _qlQuery =
        Protocol.GQLReq
          { Protocol._grOperationName = Nothing,
            Protocol._grQuery = Protocol.GQLQueryText "query { test }",
            Protocol._grVariables = Just vars
          },
      _qlGeneratedSql = fmap (\gq -> (RootFieldAlias Nothing (mkName "testField"), gq)) mGenSql,
      _qlRequestId = RequestId "test-request-id",
      _qlKind = QueryLogKindDatabase Nothing
    }

maskQueryLogSpec :: Hspec.Spec
maskQueryLogSpec =
  Hspec.describe "maskQueryLog" $ do
    Hspec.it "returns unchanged QueryLog when maskedKeys is empty" $ do
      let vars = mkVariableValues [("password", J.String "secret123"), ("name", J.String "Alice")]
          ql = mkTestQueryLog vars Nothing
          result = maskQueryLog HashSet.empty ql

      Protocol._grVariables (_qlQuery result)
        `Hspec.shouldBe` Just vars

    Hspec.it "masks matching variable values" $ do
      let vars = mkVariableValues [("password", J.String "secret123"), ("name", J.String "Alice")]
          ql = mkTestQueryLog vars Nothing
          maskedKeys = HashSet.fromList ["password"]
          result = maskQueryLog maskedKeys ql
          expected = mkVariableValues [("password", J.String "[MASKED]"), ("name", J.String "Alice")]

      Protocol._grVariables (_qlQuery result)
        `Hspec.shouldBe` Just expected

    Hspec.it "masks multiple variable keys" $ do
      let vars = mkVariableValues [("password", J.String "secret"), ("email", J.String "a@b.com"), ("name", J.String "Alice")]
          ql = mkTestQueryLog vars Nothing
          maskedKeys = HashSet.fromList ["password", "email"]
          result = maskQueryLog maskedKeys ql

      case Protocol._grVariables (_qlQuery result) of
        Nothing -> Hspec.expectationFailure "Expected generated SQL to be present"
        Just resultVars -> do
          HashMap.lookup (mkName "password") resultVars `Hspec.shouldBe` Just (J.String "[MASKED]")
          HashMap.lookup (mkName "email") resultVars `Hspec.shouldBe` Just (J.String "[MASKED]")
          HashMap.lookup (mkName "name") resultVars `Hspec.shouldBe` Just (J.String "Alice")

    Hspec.it "masks prepared_arguments in generated SQL" $ do
      let vars = mkVariableValues [("name", J.String "Alice")]
          genQuery =
            GeneratedQuery
              { _gqQueryString = "SELECT * FROM users WHERE password = $1",
                _gqPreparedArgs =
                  J.object
                    [ "password" J..= J.String "secret",
                      "name" J..= J.String "Alice"
                    ]
              }
          ql = mkTestQueryLog vars (Just genQuery)
          maskedKeys = HashSet.fromList ["password"]
          result = maskQueryLog maskedKeys ql

      case _qlGeneratedSql result of
        Just (_, gq) -> do
          let expected =
                J.object
                  [ "password" J..= J.String "[MASKED]",
                    "name" J..= J.String "Alice"
                  ]
          _gqPreparedArgs gq `Hspec.shouldBe` expected
        Nothing -> Hspec.expectationFailure "Expected generated SQL to be present"

    Hspec.it "handles Nothing variables gracefully" $ do
      let ql =
            QueryLog
              { _qlQuery =
                  Protocol.GQLReq
                    { Protocol._grOperationName = Nothing,
                      Protocol._grQuery = Protocol.GQLQueryText "query { test }",
                      Protocol._grVariables = Nothing
                    },
                _qlGeneratedSql = Nothing,
                _qlRequestId = RequestId "test-request-id",
                _qlKind = QueryLogKindDatabase Nothing
              }
          maskedKeys = HashSet.fromList ["password"]
          result = maskQueryLog maskedKeys ql

      Protocol._grVariables (_qlQuery result) `Hspec.shouldBe` Nothing

maskJsonValueSpec :: Hspec.Spec
maskJsonValueSpec =
  Hspec.describe "maskJsonValue" $ do
    Hspec.it "masks nested object keys" $ do
      let input =
            J.object
              [ "user"
                  J..= J.object
                    [ "password" J..= J.String "secret",
                      "name" J..= J.String "Alice"
                    ]
              ]
          maskedKeys = HashSet.fromList ["password"]
          result = maskJsonValue maskedKeys input
          expected =
            J.object
              [ "user"
                  J..= J.object
                    [ "password" J..= J.String "[MASKED]",
                      "name" J..= J.String "Alice"
                    ]
              ]

      result `Hspec.shouldBe` expected

    Hspec.it "masks keys inside arrays" $ do
      let input =
            J.toJSON
              [ J.object ["password" J..= J.String "a"],
                J.object ["password" J..= J.String "b"]
              ]
          maskedKeys = HashSet.fromList ["password"]
          result = maskJsonValue maskedKeys input
          expected =
            J.toJSON
              [ J.object ["password" J..= J.String "[MASKED]"],
                J.object ["password" J..= J.String "[MASKED]"]
              ]

      result `Hspec.shouldBe` expected

    Hspec.it "leaves scalar values unchanged" $ do
      let input = J.String "hello"
          maskedKeys = HashSet.fromList ["password"]

      maskJsonValue maskedKeys input `Hspec.shouldBe` input

    Hspec.it "does nothing when maskedKeys is empty" $ do
      let input = J.object ["password" J..= J.String "secret"]

      maskJsonValue HashSet.empty input `Hspec.shouldBe` input
