{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans -Wno-name-shadowing #-}

module URL (specURL) where

import Database.PG.Query.URL
import Test.Hspec
import Prelude

specURL :: Spec
specURL = do
  describe "Only the password from a postgres url is encoded if if exists" $ do
    it "None Postgres connection urls succeed" $ do
      let url = "jdbc:mysql://localhostok?user=root&password=pass&allowMultiQueries=true"
      url `shouldBe` encodeURLPassword url

    it "Postgres simple urls succeed" $ do
      let url = "postgres://localhost"
      url `shouldBe` encodeURLPassword url

    it "Postgres urls with no username, password, or database succeed" $ do
      let url = "postgres://localhost:5432"
      url `shouldBe` encodeURLPassword url

    it "Postgres urls with no username or password succeed" $ do
      let url = "postgres://localhost:5432/chinook"
      url `shouldBe` encodeURLPassword url

    it "Postgres urls with no password succeed" $ do
      let url = "postgres://user@localhost:5432/chinook"
      url `shouldBe` encodeURLPassword url

    it "Postgres urls with no password but a : succeed" $ do
      let url = "postgres://user:@localhost:5432/chinook"
      url `shouldBe` encodeURLPassword url

    it "Postgres urls with no username succeed" $ do
      let url = "postgres://:pass@localhost:5432/chinook"
      url `shouldBe` encodeURLPassword url

    it "Postgres urls with simple passwords succeed" $ do
      let url = "postgres://user:pass@localhost:5432/chinook"
      url `shouldBe` encodeURLPassword url

    it "Postgres urls with special characters passwords succeed" $ do
      let url = "postgres://user:a[:sdf($#)]@localhost:5432/chinook"
          expected = "postgres://user:a%5B%3Asdf%28%24%23%29%5D@localhost:5432/chinook"

      expected `shouldBe` encodeURLPassword url

    it "Postgres urls with special characters with @ passwords succeed" $ do
      let url = "postgres://user:a@[:sdf($@#@)]@localhost:5432/chinook"
          expected = "postgres://user:a%40%5B%3Asdf%28%24%40%23%40%29%5D@localhost:5432/chinook"

      expected `shouldBe` encodeURLPassword url
