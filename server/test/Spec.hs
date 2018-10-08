{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Spec (mkSpecs) where

import           Hasura.Prelude         hiding (get)
import           Network.Wai            (Application)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

import qualified Data.Aeson             as J
import qualified Data.Aeson.Casing      as J
import qualified Data.Aeson.TH          as J
import qualified Data.CaseInsensitive   as CI
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Yaml              as Y

type Headers = HM.HashMap T.Text T.Text

data TestCase
  = TestCase
  { tcDescription :: !T.Text
  , tcQuery       :: !J.Value
  , tcUrl         :: !T.Text
  , tcHeaders     :: !(Maybe Headers)
  , tcStatus      :: !Int
  , tcResponse    :: !(Maybe J.Value)
  -- , tcDependsOn   :: !(Maybe TestCase)
  } deriving (Show)

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''TestCase)


querySpecFiles :: [FilePath]
querySpecFiles =
  [ "create_tables.yaml"
  , "track_tables.yaml"
  , "create_author_article_relationship.yaml"
  , "create_author_article_permissions.yaml"
  , "create_address_resident_relationship_error.yaml"
  , "create_user_permission_address.yaml"
  , "reload_metadata.yaml"
  , "create_author_permission_role_admin_error.yaml"
  , "create_user_permission_test_table.yaml"
  , "all_json_queries.yaml"
  , "upsert_role_user.yaml"
  , "upsert_role_user_error.yaml"
  ]

gqlSpecFiles :: [FilePath]
gqlSpecFiles =
  [ "introspection.yaml"
  , "introspection_user_role.yaml"
  , "insert_mutation/author.yaml"
  , "insert_mutation/author_articles_nested.yaml"
  , "insert_mutation/author_articles_nested_error.yaml"
  , "simple_select_query_author.yaml"
  , "select_query_author_by_pkey.yaml"
  , "insert_mutation/article.yaml"
  , "insert_mutation/article_author_nested.yaml"
  , "insert_mutation/article_author_nested_error.yaml"
  , "insert_mutation/article_on_conflict.yaml"
  , "insert_mutation/article_on_conflict_user_role.yaml"
  , "insert_mutation/article_on_conflict_update_columns.yaml"
  , "insert_mutation/article_on_conflict_ignore.yaml"
  , "insert_mutation/article_on_conflict_ignore_constraint.yaml"
  , "insert_mutation/article_on_conflict_empty_update_columns.yaml"
  , "insert_mutation/article_on_conflict_only_constraint.yaml"
  , "insert_mutation/article_on_conflict_error_01.yaml"
  , "insert_mutation/article_on_conflict_error_02.yaml"
  , "insert_mutation/article_on_conflict_error_03.yaml"
  , "insert_mutation/person.yaml"
  , "insert_mutation/person_array.yaml"
  , "insert_mutation/order.yaml"
  , "insert_mutation/address_check_constraint_error.yaml"
  , "insert_mutation/address_not_null_constraint_error.yaml"
  , "insert_mutation/author_unique_constraint_error.yaml"
  , "insert_mutation/author_on_conflict_ignore_user_role.yaml"
  , "insert_mutation/author_on_conflict_update_user_role.yaml"
  , "nested_select_query_article.yaml"
  , "select_query_article_limit_offset.yaml"
  , "select_query_article_limit_offset_error_01.yaml"
  , "select_query_article_limit_offset_error_02.yaml"
  , "update_mutation/author.yaml"
  , "update_mutation/person_set.yaml"
  , "update_mutation/person_append.yaml"
  , "update_mutation/person_prepend.yaml"
  , "update_mutation/person_delete_key.yaml"
  , "update_mutation/person_delete_elem.yaml"
  , "update_mutation/person_delete_at_path.yaml"
  , "update_mutation/person_inc.yaml"
  , "update_mutation/person_error_01.yaml"
  , "delete_mutation/article.yaml"
  , "delete_mutation/article_returning.yaml"
  , "delete_mutation/author_foreign_key_violation.yaml"
  ]

readTestCase :: FilePath -> IO TestCase
readTestCase fpath = do
  res <- Y.decodeFileEither ("test/testcases/" ++ fpath)
  case res of
    Left e -> do
      putStrLn $ Y.prettyPrintParseException e
      error $ "Could not parse testcase YAML: " ++ fpath
    Right q -> return q

mkSpec :: TestCase -> SpecWith Application
mkSpec tc = do
  let desc = tcDescription tc
      url = tcUrl tc
      q = tcQuery tc
      mHeaders = tcHeaders tc
      statusCode = tcStatus tc
      mRespBody = tcResponse tc
      headers = maybe [] (map toHeader . HM.toList) mHeaders
      body = maybe matchAny bodyEquals $ fmap J.encode mRespBody
      resp = ResponseMatcher statusCode [] body
  it (T.unpack desc) $
    request "POST" (T.encodeUtf8 url) headers (J.encode q) `shouldRespondWith` resp
  where
    matchAny = MatchBody (\_ _ -> Nothing)
    toHeader (k, v) = (CI.mk $ T.encodeUtf8 k, T.encodeUtf8 v)


mkSpecs :: IO (SpecWith Application)
mkSpecs = do
  ddlTc <- mapM readTestCase querySpecFiles
  gqlTc <- mapM readTestCase gqlSpecFiles
  return $ do
    describe "version API" $
      it "responds with version" $
        get "/v1/version" `shouldRespondWith` 200

    describe "console endpoint" $
      it "responds with 200" $
        get "/console" `shouldRespondWith` 200

    describe "CORS test" $
      it "should respond with correct CORS headers" $
        request "OPTIONS" "/v1/version" [("Origin", "example.com")] ""
          `shouldRespondWith` 204
            {matchHeaders = ["Access-Control-Allow-Origin" <:> "example.com"]}

    describe "Query API" $ mapM_ mkSpec ddlTc

    describe "GraphQL API" $ mapM_ mkSpec gqlTc
