{-# LANGUAGE OverloadedLists #-}

module Hasura.RQL.Types.AllowlistSpec (spec) where

import Autodocodec (parseJSONViaCodec, toJSONViaCodec)
import Data.Aeson qualified as J
import Data.Aeson.Types (parseEither)
import Data.Aeson.Types qualified as J
import Data.HashMap.Strict.InsOrd.Extended qualified as InsOrdHashMap
import Data.HashSet qualified as S
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Text.Extended (toTxt)
import Data.Text.NonEmpty (mkNonEmptyTextUnsafe)
import Hasura.Prelude
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.Roles (mkRoleName)
import Test.Hspec

spec :: Spec
spec = do
  describe "allowlistAllowsQuery" $ do
    let mkCollName name = CollectionName (mkNonEmptyTextUnsafe name)
        coll1 = mkCollName "collection_1"
        coll2 = mkCollName "collection_2"
        coll3 = mkCollName "collection_3"
        coll4 = mkCollName "collection_4"

        mkQuery name body =
          ListedQuery
            (QueryName (mkNonEmptyTextUnsafe name))
            (GQLQueryWithText ("", mustJSON (J.String body)))
        lquery1 = mkQuery "query_1" "query { query_1 }"
        lquery1b = mkQuery "query_1b" "query { query_1 }"
        lquery2 = mkQuery "query_2" "query { query_2 }"
        lquery3 = mkQuery "query_3" "query { query_3 }"
        lquery4 = mkQuery "query_4" "query { query_4 }"

        query lq = unGQLQuery . getGQLQuery . _lqQuery $ lq

        mkRole = fromJust . mkRoleName
        role1 = mkRole "role_1"
        role2 = mkRole "role_2"
        role3 = mkRole "role_3"
        role4 = mkRole "role_4"

        mkCollection collName queries =
          (collName, CreateCollection collName (CollectionDef queries) Nothing)
        collections =
          InsOrdHashMap.fromList
            [ mkCollection coll1 [lquery1],
              mkCollection coll2 [lquery2],
              mkCollection coll3 [lquery3, lquery1b],
              mkCollection coll4 [lquery2, lquery1]
            ]

        mkAllowGlobal collName =
          (collName, AllowlistEntry collName AllowlistScopeGlobal)
        mkAllowRoles collName roles =
          (collName, AllowlistEntry collName (AllowlistScopeRoles roles))
        emptyAllowlist = mempty
        complexAllowlist =
          InsOrdHashMap.fromList
            [ mkAllowGlobal coll2,
              mkAllowRoles coll1 (role1 NE.:| [role4]),
              mkAllowRoles coll3 (role2 NE.:| [role3]),
              mkAllowRoles coll4 (role4 NE.:| [])
            ]

        allowed :: InlinedAllowlist -> AllowlistMode -> HashSet (Text, Text)
        allowed allowlist mode =
          S.fromList
            [ (toTxt role, toTxt (_lqName lquery))
              | role <- [role1, role2, role3, role4],
                lquery <- [lquery1, lquery1b, lquery2, lquery3, lquery4],
                allowlistAllowsQuery allowlist mode role (query lquery)
            ]

    it "denies queries when the allowlist is empty" $ do
      let allowlist = inlineAllowlist collections emptyAllowlist
          mode = AllowlistModeFull
      allowed allowlist mode `shouldBe` S.empty

    it "allows queries appropriately for a complex allowlist" $ do
      let allowlist = inlineAllowlist collections complexAllowlist
          mode = AllowlistModeFull
      allowed allowlist mode
        `shouldBe` S.fromList
          [ ("role_1", "query_1"), -- via coll 1
            ("role_1", "query_1b"), -- also via coll 1 (because same query)
            ("role_1", "query_2"), -- via coll 2 (global)
            ("role_2", "query_1"), -- via coll 3 (query_1b)
            ("role_2", "query_1b"), -- via coll 3
            ("role_2", "query_2"), -- via coll 2
            ("role_2", "query_3"), -- via coll 3
            ("role_3", "query_1"), -- role 3 has the same permissions as role 2
            ("role_3", "query_1b"),
            ("role_3", "query_2"),
            ("role_3", "query_3"),
            ("role_4", "query_1"), -- via coll 1 or coll 4
            ("role_4", "query_1b"),
            ("role_4", "query_2") -- via coll 2 or coll 4
          ]

    it "ignores role-based allowlist in global mode" $ do
      let allowlist = inlineAllowlist collections complexAllowlist
          mode = AllowlistModeGlobalOnly
      allowed allowlist mode
        `shouldBe` S.fromList
          [ ("role_1", "query_2"),
            ("role_2", "query_2"),
            ("role_3", "query_2"),
            ("role_4", "query_2")
          ]

    it "round-trips roles when serializing via codecs" do
      let expected =
            maybeToEither "nonempty"
              $ AllowlistScopeRoles
              <$> traverse mkRoleName ["viewer", "admin"]
      let json = toJSONViaCodec <$> expected
      let actual = parseEither parseJSONViaCodec =<< json
      actual `shouldBe` expected

mustJSON :: (J.FromJSON a) => J.Value -> a
mustJSON v = case J.parseEither J.parseJSON v of
  Left err -> error err
  Right x -> x
