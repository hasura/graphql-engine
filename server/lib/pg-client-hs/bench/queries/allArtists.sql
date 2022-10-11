SELECT coalesce(
  json_agg(
    (SELECT  "e"  FROM  (SELECT  "r"."name" AS "name", "r"."id" AS "id") AS "e")
  ),
  '[]'
  )
  FROM
  (SELECT  "name" AS "name", "id" AS "id"
     FROM "public"."artists"
  ) AS "r"
