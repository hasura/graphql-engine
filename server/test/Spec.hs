{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Spec (mkSpecs) where

import           Hasura.Prelude     hiding (get)
import           Network.Wai        (Application)
import           Test.Hspec
import           Test.Hspec.Wai

import qualified Data.Aeson         as J
import qualified Data.Aeson.Casing  as J
import qualified Data.Aeson.TH      as J
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml          as Y


data TestCase
  = TestCase
  { tcDescription :: !T.Text
  , tcQuery       :: !J.Value
  , tcUrl         :: !T.Text
  , tcStatus      :: !Int
  } deriving (Show)

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''TestCase)

querySpecFiles :: [FilePath]
querySpecFiles =
  [ "create_tables.yaml"
  , "track_tables.yaml"
  , "create_author_article_relationship.yaml"
  ]

gqlSpecFiles :: [FilePath]
gqlSpecFiles =
  [ "introspection.yaml"
  , "insert_mutation_author.yaml"
  , "simple_select_query_author.yaml"
  , "insert_mutation_article.yaml"
  , "insert_mutation_article_on_conflict.yaml"
  , "insert_mutation_article_on_conflict_ignore.yaml"
  , "insert_mutation_article_on_conflict_ignore_constraint.yaml"
  , "insert_mutation_article_on_conflict_error_01.yaml"
  , "insert_mutation_article_on_conflict_error_02.yaml"
  , "insert_mutation_article_on_conflict_error_03.yaml"
  , "nested_select_query_article.yaml"
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
      respStatus = (fromIntegral $ tcStatus tc) :: ResponseMatcher
  it (T.unpack desc) $ do
    post (T.encodeUtf8 url) (J.encode q) `shouldRespondWith` respStatus

mkSpecs :: IO (SpecWith Application)
mkSpecs = do
  ddlTc <- mapM readTestCase querySpecFiles
  gqlTc <- mapM readTestCase gqlSpecFiles
  return $ do
    describe "version API" $
      it "responds with version" $
        get "/v1/version" `shouldRespondWith` 200

    describe "Query API" $ mapM_ mkSpec ddlTc

    describe "GraphQL API" $ mapM_ mkSpec gqlTc
