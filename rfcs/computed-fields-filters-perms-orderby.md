Note: This RFC assumes computed fields for **Postgres** backend.

OSS issue: https://github.com/hasura/graphql-engine/issues/3772

Before we proceed to the specification, we shall address the known limitation


### Limitation

All the undermentioned implementations assumes computed field function has table row input and/or
session (`json` type) input arguments. But we support computed fields with functions having multiple arguments.
Input values for additional arguments are provided via GraphQL field [arguments](https://spec.graphql.org/June2018/#sec-Language.Arguments).
For example, defining `get_articles` computed field for `author` table with `get_articles(table_input author, search text)` SQL
function to return author's articles containing `search` keyword.
```graphql
query {
    author {
        id
        name
        get_articles(search: "Covid 19"){
            title
            content
        }
    }
}
```
It is complex to support such additional function inputs via GraphQL arguments.
So, here I'm assuming the following limitation:

**Allow filter/order by/permission for computed fields with no input arguments other than table row and session json input types**

### Filter

Reference OSS ticket: https://github.com/hasura/graphql-engine/issues/7100

Currently, we only support table columns and relationships (object and array) to filter rows in any GraphQL query. For example, to fetch an author whose `id` is `1` the query would be (using column in filter)

```graphql
query {
  authors(where: {id: {_eq: 1}}){
    id
    first_name
    last_name
  }
}
```

Similarly, to fetch articles whose `author`'s `first_name` is 'Bob', the query would be (using object relation in filter)

```graphql
query {
  articles(where: {author: {first_name: {_eq: "Bob"}}}){
    id
    title
  }
}
```

Now, I defined a computed field `full_name` to the `author` table using the following SQL function

```sql
CREATE OR REPLACE FUNCTION public.full_name(author_table author)
 RETURNS text
 LANGUAGE sql
 STABLE
AS $function$
  SELECT author_table.first_name || ' ' || author_table.last_name
$function$
```

I should able to fetch an author whose `full_name` is 'Bob Morley'

```graphql
query {
  authors(where: {full_name: {_eq: "Bob Morley"}}){
    id
    full_name
  }
}
```

The above query isn't possible currently and hence, we need to support computed fields in filter expression (`where` clause).

#### Approach

```sql
SELECT "base".* FROM "authors" AS "base" WHERE "full_name"("base") = 'Bob Morley'
```

Simply, our approach is to use the SQL function in the `WHERE` expression of the generated SQL. So, aforementioned GraphQL query
translates to
```sql
SELECT
  coalesce(json_agg("root"), '[]') AS "root"
FROM
  (
    SELECT
      row_to_json(
        (
          SELECT
            "_2_e"
          FROM
            (
              SELECT
                "_1_root.base"."first_name" AS "first_name",
                "_1_root.base"."last_name" AS "last_name",
                "public"."user_full_name"("_1_root.base") AS "full_name"
            ) AS "_2_e"
        )
      ) AS "root"
    FROM
      (
        SELECT
          *
        FROM
          "public"."user" AS "_0_base"
        WHERE
          (
            ("public"."user_full_name"("_0_base".*)) = (('Bob Morley') :: text)
          )
      ) AS "_1_root.base"
  ) AS "_3_root"
```

### Permission

Reference OSS ticket: https://github.com/hasura/graphql-engine/issues/7102

Just like using computed fields in `where` expression, similarly we need have an option to include computed fields in permission
`check` and `filter` expression

For example, I need to define a select permission on `author` table whose `full_name` is like 'Bob'

```json
- type: create_select_permission
  args:
    table:
      name: author
    role: user
    permission:
      columns: '*'
      filter:
        full_name:
            _like: 'Bob'
```

#### Approach

Similar to `filtering` approach, we generate `where` expression with the `filter`/`check` expression provided
in the permission metadata definition

### Order by

Reference OSS ticket: https://github.com/hasura/graphql-engine/issues/7103

Enable using computed fields in order by expresssion. For example, fetch authors ordering by `full_name`

```graphql
query {
  authors(order_by: {full_name: desc}){
    id
    first_name
  }
}
```

#### Approach

We extract computed fields with proper aliases and use the same in the `ORDER BY` SQL expression.
The generated SQL will look like

```sql
SELECT
  computed_field_function("table_alias") AS "computed_field_alias"
 (SELECT * FROM our_table) AS "table_alias"
ORDER BY
  "computed_field_alias" DESC
```
