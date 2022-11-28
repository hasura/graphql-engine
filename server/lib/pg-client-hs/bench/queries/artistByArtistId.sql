SELECT coalesce(
  json_agg(
    (SELECT  "e"  FROM  (SELECT  "r"."name" AS "name", "r"."id" AS "id") AS "e")
  ),
  '[]'
  )
  FROM
  (SELECT  "name" AS "name", "id" AS "id"
     FROM "public"."artists"
    WHERE ( ('true') AND (('true') AND ((((("public"."artists"."id") = ($1))
          OR ((("public"."artists"."id") IS NULL)
          AND (($1) IS NULL))) AND ('true')) AND ('true'))))
  ) AS "r"
