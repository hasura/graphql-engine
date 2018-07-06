{-# LANGUAGE OverloadedStrings #-}

module Spec (specs) where

import           Hasura.Prelude hiding (get)
import           Network.Wai    (Application)
import           Test.Hspec
import           Test.Hspec.Wai

import qualified Data.Aeson     as J

import           Queries

specs :: SpecWith Application
specs = describe "test APIs" $ do

  describe "version API" $
    it "responds with version" $
      get "/v1/version" `shouldRespondWith` 200

  describe "Query API" $ do
    it "runs SQL query to create a table" $
      post "/v1/query" (J.encode createTable) `shouldRespondWith` 200
    it "tracks the above created table" $
      post "/v1/query" (J.encode trackTable) `shouldRespondWith` 200
