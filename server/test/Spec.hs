{-# LANGUAGE OverloadedStrings #-}

module Spec (specs) where

import           Control.Monad.IO.Class (liftIO)
import           Hasura.Prelude         hiding (get)
import           Network.Wai            (Application)
import           Test.Hspec
import           Test.Hspec.Wai

import qualified Data.Aeson             as J
import qualified Data.Yaml              as Y


querySpecFiles :: [(String, FilePath)]
querySpecFiles =
  [ ("runs bulk SQL query to create tables", "create_tables.yaml")
  , ("tracks the above created tables", "track_tables.yaml")
  , ("creates relationships", "create_author_article_relationship.yaml")
  ]

gqlSpecFiles :: [(String, FilePath)]
gqlSpecFiles =
  [ ("inserts author data via GraphQL mutation", "insert_mutation_author.yaml")
  , ("simple GraphQL object query on author", "simple_select_query_author.yaml")
  , ("inserts article data via GraphQL mutation", "insert_mutation_article.yaml")
  , ("nested select on article", "nested_select_query_article.yaml")
  ]

readTestCase :: FilePath -> IO J.Value
readTestCase fpath = do
  res <- Y.decodeFileEither ("test/testcases/" ++ fpath)
  case res of
    Left e  -> do
      putStrLn $ Y.prettyPrintParseException e
      return . J.String $ "error"
    Right q -> return q

mkRQLSpec :: String -> FilePath -> SpecWith Application
mkRQLSpec desc filepath =
  it desc $ do
    tc <- liftIO $ readTestCase filepath
    post "/v1/query" (J.encode tc) `shouldRespondWith` 200

mkGQLSpec :: String -> FilePath -> SpecWith Application
mkGQLSpec desc filepath =
  it desc $ do
    tc <- liftIO $ readTestCase filepath
    post "/v1alpha1/graphql" (J.encode tc) `shouldRespondWith` 200

specs :: SpecWith Application
specs = describe "test APIs" $ do
  describe "version API" $
    it "responds with version" $ do
      get "/v1/version" `shouldRespondWith` 200

  describe "Query API" $ do
    forM_ querySpecFiles $ \(desc, fpath) -> do
      mkRQLSpec desc fpath

  describe "GraphQL API" $ do
    forM_ gqlSpecFiles $ \(desc, fpath) -> do
      mkGQLSpec desc fpath
