-- | Test identifiers with names longer than 63 characters have an md5 hash prefix.
--   Tests the "Hasura.Backends.Postgres.SQL.RenameIdentifiers" module.
module Hasura.Backends.Postgres.SQL.Select.RenameIdentifiersSpec (spec) where

import Hasura.Backends.Postgres.SQL.EDSL
import Hasura.Backends.Postgres.SQL.RenameIdentifiers
import Hasura.Prelude hiding (exp)
import Test.Hspec

spec :: Spec
spec = do
  it "empty is empty"
    $ shouldBe
      (renameIdentifiers mkSelect)
      mkSelect
  literalQueries
  simpleQueries

literalQueries :: Spec
literalQueries =
  describe "literal queries" $ do
    it "no alias" $ do
      let noAlias =
            mkSelect
              { selExtr = [Extractor (SELit "1") Nothing]
              }
      shouldBe
        (renameIdentifiers noAlias)
        noAlias

    it "top-level extractor not modified" $ do
      let noAlias =
            mkSelect
              { selExtr = [Extractor (SELit "1") (Just $ ColumnAlias $ Identifier "one")]
              }
      shouldBe
        (renameIdentifiers noAlias)
        noAlias

simpleQueries :: Spec
simpleQueries =
  describe "simple queries" $ do
    it "simple query" $ do
      -- SELECT coalesce(json_agg("root"), '[]') AS "root"
      -- FROM
      --   ( SELECT row_to_json(
      --        (SELECT "e" FROM (SELECT "root.base"."name" AS "name", "root.base"."age" AS "age") AS "e")
      --      ) AS "root"
      --      FROM (SELECT * FROM "public"."user") AS "root.base"
      --   ) AS "root"
      let query e' rootbase' root' =
            mkSelect
              { selExtr = rootExtractor_,
                selFrom =
                  from_
                    [ mkSelect
                        { selExtr = [row_to_json_ [selectIdentifiers_ e' rootbase' ["name", "age"]] `asC_` "root"],
                          selFrom = from_ [selectStar_ "public" "user" `as'_` rootbase']
                        }
                        `as'_` root'
                    ]
              }
          (input, expected) =
            ( query "e" "root.base" "root",
              query "_e" "_root.base" "_root"
            )
      renameIdentifiers input `shouldBe` expected

    it "simple query with where" $ do
      -- SELECT coalesce(json_agg("root"), '[]') AS "root"
      -- FROM
      --   ( SELECT row_to_json(
      --      (SELECT "e" FROM (SELECT "root.base"."name" AS "name", "root.base"."age" AS "age") AS "e")
      --     ) AS "root"
      --     FROM (SELECT * FROM "public"."user") AS "root.base"
      --     WHERE (("public"."user"."id") = (($2)::integer))
      --   ) AS "root"
      let query e' rootbase' root' =
            mkSelect
              { selExtr = rootExtractor_,
                selFrom =
                  from_
                    [ mkSelect
                        { selExtr = [row_to_json_ [selectIdentifiers_ e' rootbase' ["name", "age"]] `asC_` "root"],
                          selFrom = from_ [selectStar_ "public" "user" `as'_` rootbase'],
                          selWhere = where_ $ stcolumn_ "public" "user" "id" `eq_` int_
                        }
                        `as'_` root'
                    ]
              }
          (input, expected) =
            ( query "e" "root.base" "root",
              query "_e" "_root.base" "_root"
            )
      renameIdentifiers input `shouldBe` expected

    it "simple query with relationship" $ do
      -- SELECT coalesce(json_agg("root"), '[]') AS "root"
      -- FROM
      --   ( SELECT row_to_json((
      --       SELECT "e" FROM (SELECT "root.base"."id" AS "id", "root.base"."author" AS "author") AS "e"
      --     )) AS "root"
      --     FROM (SELECT * FROM "public"."article") AS "root.base"
      --     LEFT OUTER JOIN LATERAL
      --       ( SELECT row_to_json((
      --           SELECT "e"
      --           FROM (SELECT "root.or.author.base"."name" AS "name") AS "e"
      --         )) AS "author"
      --         FROM
      --           ( SELECT *
      --             FROM "public"."author"
      --             WHERE (("root.base"."id") = ("id"))
      --             LIMIT 1
      --           ) AS "root.or.author.base"
      --       ) AS "root.or.author"
      --     ON ('true')
      --   ) AS "root"
      let query e1' root_base' e2' root_or_author_base' root_or_author' root' =
            mkSelect
              { selExtr = rootExtractor_,
                selFrom =
                  from_
                    [ mkSelect
                        { selExtr = [row_to_json_ [selectIdentifiers_ e1' root_base' ["id", "author"]] `asC_` "root"],
                          selFrom =
                            from_
                              $ lateralLeftJoin_
                                (selectStar_ "public" "article" `as'_` root_base')
                                ( mkSelect
                                    { selExtr =
                                        [row_to_json_ [selectIdentifiers_ e2' root_or_author_base' ["name"]] `asC_` "author"],
                                      selFrom =
                                        from_
                                          [ (selectStar_ "public" "author")
                                              { selWhere = where_ $ tcolumn_ root_base' "id" `eq_` iden_ "id",
                                                selLimit = limit1_
                                              }
                                              `as'_` root_or_author_base'
                                          ]
                                    }
                                    `as'_` root_or_author'
                                )
                        }
                        `as'_` root'
                    ]
              }
          (input, expected) =
            ( query "e" "root.base" "e" "root.or.author.base" "root.or.author" "root",
              query "_e" "_root.base" "_e" "_root.or.author.base" "_root.or.author" "_root"
            )

      renameIdentifiers input `shouldBe` expected

    it "simple query with relationship and outer orderby" $ do
      -- SELECT coalesce(json_agg("root"), '[]') AS "root"
      -- FROM
      --   ( SELECT row_to_json((
      --       SELECT "e"
      --       FROM (SELECT "root.base"."id" AS "id",
      --                    "root.base"."author_with_a_very_long_name_that_is_almost_63_characters_long" AS "author_with_a_very_long_name_that_is_almost_63_characters_long"
      --            ) AS "e"
      --     )) AS "root"
      --     FROM (SELECT * FROM "public"."article") AS "root.base"
      --     LEFT OUTER JOIN LATERAL
      --       ( SELECT row_to_json((
      --           SELECT "e"
      --           FROM (SELECT "root.or.author_with_a_very_long_name_that_is_almost_63_characters_long.base"."name" AS "name") AS "e"
      --         )) AS "author_with_a_very_long_name_that_is_almost_63_characters_long"
      --         FROM
      --           ( SELECT *
      --             FROM "public"."author_with_a_very_long_name_that_is_almost_63_characters_long"
      --             WHERE (("root.base"."id") = ("id"))
      --             LIMIT 1
      --           ) AS "root.or.author_with_a_very_long_name_that_is_almost_63_characters_long.base"
      --       ) AS "root_or_author_with_a_very_long_name_that_is_almost_63_characters_long"
      --     ON ('true')
      --   ) AS "root"
      let query e1' root_base' e2' root_or_author_with_a_very_long_name_that_is_almost_63_characters_long_base' root_or_author_with_a_very_long_name_that_is_almost_63_characters_long' root' key1' key2' =
            mkSelect
              { selExtr = extractorOrd_ "root" [asc_ key1', asc_ key2'] `asC_` "root",
                selFrom =
                  from_
                    [ mkSelect
                        { selExtr = [row_to_json_ [selectIdentifiers_ e1' root_base' ["id", "author_with_a_very_long_name_that_is_almost_63_characters_long"]] `asC_` "root"],
                          selFrom =
                            from_
                              $ lateralLeftJoin_
                                (selectStar_ "public" "article" `as'_` root_base')
                                ( mkSelect
                                    { selExtr =
                                        [row_to_json_ [selectIdentifiers_ e2' root_base' ["name", "id"]] `asC_` "author_with_a_very_long_name_that_is_almost_63_characters_long"],
                                      selFrom =
                                        from_
                                          [ (selectStar_ "public" "author_with_a_very_long_name_that_is_almost_63_characters_long")
                                              { selWhere = where_ $ tcolumn_ root_base' "id" `eq_` iden_ "id",
                                                selLimit = limit1_
                                              }
                                              `as'_` root_or_author_with_a_very_long_name_that_is_almost_63_characters_long_base'
                                          ]
                                    }
                                    `as'_` root_or_author_with_a_very_long_name_that_is_almost_63_characters_long'
                                ),
                          selOrderBy = orderby_ [asc_ key1', asc_ key2']
                        }
                        `as'_` root'
                    ]
              }

          (input, expected) =
            ( query
                "e"
                "root.base"
                "e"
                "root.or.author_with_a_very_long_name_that_is_almost_63_characters_long.base"
                "root_or_author_with_a_very_long_name_that_is_almost_63_characters_long"
                "root"
                "root.or.author_with_a_very_long_name_that_is_almost_63_characters_long.pg.name"
                "root.or.author_with_a_very_long_name_that_is_almost_63_characters_long.pg.id",
              query
                "_e"
                "_root.base"
                "_e"
                "md5_c4da2dba0563dfc8fb91b8480b624b93__root.or.author_with_a_very_long_name_that_is_almost_63_characters_long.base"
                "md5_03bb88dfd239383315424d089039209c__root_or_author_with_a_very_long_name_that_is_almost_63_characters_long"
                "_root"
                "md5_0a05b89ec186374db29e23e032db5417_root.or.author_with_a_very_long_name_that_is_almost_63_characters_long.pg.name"
                "md5_bd48f5a68cba955aa3eb9edb769b8596_root.or.author_with_a_very_long_name_that_is_almost_63_characters_long.pg.id"
            )

      renameIdentifiers input `shouldBe` expected

    it "simple query with relationship and inner orderby" $ do
      -- SELECT coalesce(json_agg("root"), '[]') AS "root"
      -- FROM
      --   ( SELECT row_to_json((
      --       SELECT "e"
      --       FROM (SELECT "root.base"."id" AS "id", "root.base"."author" AS "author") AS "e"
      --     )) AS "root"
      --     FROM
      --       ( SELECT * FROM "public"."author") AS "root.base"
      --     LEFT OUTER JOIN LATERAL
      --       ( SELECT coalesce(json_agg(
      --           "these_are_such_nice_articles_honest"
      --           ORDER BY
      --           "root.ar.root.these_are_such_nice_articles_honest.pg.content" ASC NULLS LAST,
      --           "root.ar.root.these_are_such_nice_articles_honest.pg.published_on" ASC NULLS LAST
      --         ), '[]') AS "these_are_such_nice_articles_honest"
      --         FROM
      --           ( SELECT
      --               "root.ar.root.these_are_such_nice_articles_honest.base"."content" AS "root.ar.root.these_are_such_nice_articles_honest.pg.content",
      --               row_to_json((
      --                 SELECT "e"
      --                 FROM
      --                 ( SELECT
      --                     "root.ar.root.these_are_such_nice_articles_honest.base"."title" AS "title",
      --                     "root.ar.root.these_are_such_nice_articles_honest.base"."content" AS "content"
      --                 ) AS "e"
      --               )) AS "these_are_such_nice_articles_honest",
      --               "root.ar.root.these_are_such_nice_articles_honest.base"."published_on" AS "root.ar.root.these_are_such_nice_articles_honest.pg.published_on"
      --              FROM
      --                ( SELECT *
      --                  FROM "public"."article"
      --                  WHERE (("root.base"."id") = ("author_id"))
      --                  ORDER BY "content" ASC NULLS LAST, "published_on" ASC NULLS LAST
      --                ) AS "root.ar.root.these_are_such_nice_articles_honest"
      --           ) AS "root.ar.root.these_are_such_nice_articles_honest"
      --         ORDER BY "root.ar.root.these_are_such_nice_articles_honest.pg.content" ASC NULLS LAST,
      --                  "root.or.these_are_such_nice_articles_honest.pg.published_on" ASC NULLS LAST
      --       ) AS "root.ar.root.these_are_such_nice_articles_honest"
      --     ON ('true')
      --   ) AS "root"
      let query e1' root_base' e2' root_ar_root_these_are_such_nice_articles_honest1' root_ar_root_these_are_such_nice_articles_honest2' root_ar_root_these_are_such_nice_articles_honest3' asPublishedOn' root' key1' key2' =
            mkSelect
              { selExtr = rootExtractor_,
                selFrom =
                  from_
                    [ mkSelect
                        { selExtr = [row_to_json_ [selectIdentifiers_ e1' root_base' ["id", "author"]] `asC_` "root"],
                          selFrom =
                            from_
                              $ lateralLeftJoin_
                                (selectStar_ "public" "author" `as'_` root_base')
                                ( mkSelect
                                    { selExtr =
                                        extractorOrd_
                                          "these_are_such_nice_articles_honest"
                                          [ asc_ key1',
                                            asc_ key2'
                                          ]
                                          `asC_` "these_are_such_nice_articles_honest",
                                      selFrom =
                                        from_
                                          [ mkSelect
                                              { selExtr =
                                                  [ tcolumn_ "root.ar.root.these_are_such_nice_articles_honest.base" "content"
                                                      `asE_` "root.ar.root.these_are_such_nice_articles_honest.pg.content",
                                                    row_to_json_
                                                      [ selectIdentifiersFromExp_
                                                          "e"
                                                          "root.ar.root.these_are_such_nice_articles_honest.base"
                                                          ["title", "content"]
                                                          e2'
                                                      ]
                                                      `asC_` "these_are_such_nice_articles_honest",
                                                    tcolumn_ "root.ar.root.these_are_such_nice_articles_honest.base" "published_on"
                                                      `asE_` asPublishedOn'
                                                  ],
                                                selFrom =
                                                  from_
                                                    [ (selectStar_ "public" "article")
                                                        { selWhere = where_ $ tcolumn_ root_base' "id" `eq_` iden_ "author_id",
                                                          selOrderBy = orderby_ [asc_ "content", asc_ "published_on"]
                                                        }
                                                        `as'_` root_ar_root_these_are_such_nice_articles_honest1'
                                                    ]
                                              }
                                              `as'_` root_ar_root_these_are_such_nice_articles_honest2'
                                          ],
                                      selOrderBy = orderby_ [asc_ key1', asc_ key2']
                                    }
                                    `as'_` root_ar_root_these_are_such_nice_articles_honest3'
                                )
                        }
                        `as'_` root'
                    ]
              }

          (input, expected) =
            ( query
                "e"
                "root.base"
                "e"
                "root.ar.root.these_are_such_nice_articles_honest"
                "root.ar.root.these_are_such_nice_articles_honest"
                "root.ar.root.these_are_such_nice_articles_honest"
                "root.ar.root.these_are_such_nice_articles_honest.pg.published_on"
                "root"
                "root.ar.root.these_are_such_nice_articles_honest.pg.content"
                "root.ar.root.these_are_such_nice_articles_honest.pg.published_on",
              query
                "_e"
                "_root.base"
                "_e"
                "_root.ar.root.these_are_such_nice_articles_honest"
                "_root.ar.root.these_are_such_nice_articles_honest"
                "_root.ar.root.these_are_such_nice_articles_honest"
                "md5_ac1b16cded5dd05d0da14065dd60aff0_root.ar.root.these_are_such_nice_articles_honest.pg.published_on"
                "_root"
                "root.ar.root.these_are_such_nice_articles_honest.pg.content"
                "md5_ac1b16cded5dd05d0da14065dd60aff0_root.ar.root.these_are_such_nice_articles_honest.pg.published_on"
            )

      renameIdentifiers input `shouldBe` expected
