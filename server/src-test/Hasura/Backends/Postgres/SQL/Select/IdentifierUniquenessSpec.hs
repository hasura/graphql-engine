-- | Tests for stuff under Hasura.Eventing hierarchy
module Hasura.Backends.Postgres.SQL.Select.IdentifierUniquenessSpec (spec) where

import Hasura.Backends.Postgres.SQL.EDSL
import Hasura.Backends.Postgres.SQL.IdentifierUniqueness
import Hasura.Prelude hiding (exp)
import Test.Hspec

spec :: Spec
spec = do
  it "empty is empty" $
    shouldBe
      (prefixNumToAliases mkSelect)
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
        (prefixNumToAliases noAlias)
        noAlias

    it "top-level extractor not modified" $ do
      let noAlias =
            mkSelect
              { selExtr = [Extractor (SELit "1") (Just $ Alias $ Identifier "one")]
              }
      shouldBe
        (prefixNumToAliases noAlias)
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
                        { selExtr = [row_to_json_ [selectIdentifiers_ e' rootbase' ["name", "age"]] `as_` "root"],
                          selFrom = from_ [selectStar_ "public" "user" `as'_` rootbase']
                        }
                        `as'_` root'
                    ]
              }
          (input, expected) =
            ( query "e" "root.base" "root",
              query "_1_e" "_0_root.base" "_2_root"
            )
      prefixNumToAliases input `shouldBe` expected

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
                        { selExtr = [row_to_json_ [selectIdentifiers_ e' rootbase' ["name", "age"]] `as_` "root"],
                          selFrom = from_ [selectStar_ "public" "user" `as'_` rootbase'],
                          selWhere = where_ $ stcolumn_ "public" "user" "id" `eq_` int_
                        }
                        `as'_` root'
                    ]
              }
          (input, expected) =
            ( query "e" "root.base" "root",
              query "_1_e" "_0_root.base" "_2_root"
            )
      prefixNumToAliases input `shouldBe` expected

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
                        { selExtr = [row_to_json_ [selectIdentifiers_ e1' root_base' ["id", "author"]] `as_` "root"],
                          selFrom =
                            from_ $
                              lateralLeftJoin_
                                (selectStar_ "public" "article" `as'_` root_base')
                                ( mkSelect
                                    { selExtr =
                                        [row_to_json_ [selectIdentifiers_ e2' root_or_author_base' ["name"]] `as_` "author"],
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
              query "_4_e" "_0_root.base" "_2_e" "_1_root.or.author.base" "_3_root.or.author" "_5_root"
            )

      prefixNumToAliases input `shouldBe` expected

    it "simple query with relationship and outer orderby" $ do
      -- SELECT coalesce(json_agg("root"), '[]') AS "root"
      -- FROM
      --   ( SELECT row_to_json((
      --       SELECT "e"
      --       FROM (SELECT "root.base"."id" AS "id", "root.base"."author" AS "author") AS "e"
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
      --       ) AS "root_or_author"
      --     ON ('true')
      --   ) AS "root"
      let query e1' root_base' e2' root_or_author_base' root_or_author' root' =
            mkSelect
              { selExtr = extractorOrd_ "root" [asc_ "root.or.author.pg.name", asc_ "root.or.author.pg.id"] `as_` "root",
                selFrom =
                  from_
                    [ mkSelect
                        { selExtr = [row_to_json_ [selectIdentifiers_ e1' root_base' ["id", "author"]] `as_` "root"],
                          selFrom =
                            from_ $
                              lateralLeftJoin_
                                (selectStar_ "public" "article" `as'_` root_base')
                                ( mkSelect
                                    { selExtr =
                                        [row_to_json_ [selectIdentifiers_ e2' root_base' ["name", "id"]] `as_` "author"],
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
                                ),
                          selOrderBy = orderby_ [asc_ "root.or.author.pg.name", asc_ "root.or.author.pg.id"]
                        }
                        `as'_` root'
                    ]
              }

          (input, expected) =
            ( query "e" "root.base" "e" "root.or.author.base" "root_or_author" "root",
              query "_4_e" "_0_root.base" "_2_e" "_1_root.or.author.base" "_3_root_or_author" "_5_root"
            )

      prefixNumToAliases input `shouldBe` expected

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
      --           "articles"
      --           ORDER BY
      --           "root.ar.root.articles.pg.content" ASC NULLS LAST,
      --           "root.ar.root.articles.pg.published_on" ASC NULLS LAST
      --         ), '[]') AS "articles"
      --         FROM
      --           ( SELECT
      --               "root.ar.root.articles.base"."content" AS "root.ar.root.articles.pg.content",
      --               row_to_json((
      --                 SELECT "e"
      --                 FROM
      --                 ( SELECT
      --                     "root.ar.root.articles.base"."title" AS "title",
      --                     "root.ar.root.articles.base"."content" AS "content"
      --                 ) AS "e"
      --               )) AS "articles",
      --               "root.ar.root.articles.base"."published_on" AS "root.ar.root.articles.pg.published_on"
      --              FROM
      --                ( SELECT *
      --                  FROM "public"."article"
      --                  WHERE (("root.base"."id") = ("author_id"))
      --                  ORDER BY "content" ASC NULLS LAST, "published_on" ASC NULLS LAST
      --                ) AS "root.ar.root.articles"
      --           ) AS "root.ar.root.articles"
      --         ORDER BY "root.ar.root.articles.pg.content" ASC NULLS LAST,
      --                  "root.or.articles.pg.published_on" ASC NULLS LAST
      --       ) AS "root.ar.root.articles"
      --     ON ('true')
      --   ) AS "root"
      let query e1' root_base' e2' root_ar_root_articles1' root_ar_root_articles2' root_ar_root_articles3' root' =
            mkSelect
              { selExtr = rootExtractor_,
                selFrom =
                  from_
                    [ mkSelect
                        { selExtr = [row_to_json_ [selectIdentifiers_ e1' root_base' ["id", "author"]] `as_` "root"],
                          selFrom =
                            from_ $
                              lateralLeftJoin_
                                (selectStar_ "public" "author" `as'_` root_base')
                                ( mkSelect
                                    { selExtr =
                                        extractorOrd_
                                          "articles"
                                          [ asc_ "root.ar.root.articles.pg.content",
                                            asc_ "root.ar.root.articles.pg.published_on"
                                          ]
                                          `as_` "articles",
                                      selFrom =
                                        from_
                                          [ mkSelect
                                              { selExtr =
                                                  [ tcolumn_ "root.ar.root.articles.base" "content"
                                                      `asE_` "root.ar.root.articles.pg.content",
                                                    row_to_json_
                                                      [ selectIdentifiersFromExp_
                                                          "e"
                                                          "root.ar.root.articles.base"
                                                          ["title", "content"]
                                                          e2'
                                                      ]
                                                      `as_` "articles",
                                                    tcolumn_ "root.ar.root.articles.base" "published_on"
                                                      `asE_` "root.ar.root.articles.pg.published_on"
                                                  ],
                                                selFrom =
                                                  from_
                                                    [ (selectStar_ "public" "article")
                                                        { selWhere = where_ $ tcolumn_ root_base' "id" `eq_` iden_ "author_id",
                                                          selOrderBy = orderby_ [asc_ "content", asc_ "published_on"]
                                                        }
                                                        `as'_` root_ar_root_articles1'
                                                    ]
                                              }
                                              `as'_` root_ar_root_articles2'
                                          ],
                                      selOrderBy = orderby_ [asc_ "root.ar.root.articles.pg.content", asc_ "root.or.articles.pg.published_on"]
                                    }
                                    `as'_` root_ar_root_articles3'
                                )
                        }
                        `as'_` root'
                    ]
              }

          (input, expected) =
            ( query "e" "root.base" "e" "root.ar.root.articles" "root.ar.root.articles" "root.ar.root.articles" "root",
              query "_5_e" "_0_root.base" "_2_e" "_1_root.ar.root.articles" "_3_root.ar.root.articles" "_4_root.ar.root.articles" "_6_root"
            )

      prefixNumToAliases input `shouldBe` expected
