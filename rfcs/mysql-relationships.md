```
---
authors: Evie Ciobanu <evie@hasura.io>
state: draft
---
```

# MySQL Relationships

As the name suggests, relationships are a core feature of relational
databases. Using relationships between entities, we can query or update
structured data as well as ensure data consistency.

## Description

Since relationships are such a key feature of any relational database,
including MySQL, the Hasura engine is expected to be able to handle
them as well.

Relationships are either object or array. Object relationships represent
a single related entity (e.g., an article is only published by one
author), whereas array relationships may have any number of related entities
(e.g., an author may have any number of published articles).

### Success

This feature allows us to track, untrack, and query both object and array
relationships for MySQL sources.

Additionally, the `TestRelationships` test suite should pass on MySQL sources
(https://github.com/hasura/graphql-engine-mono/blob/6905d5914c8a698445c0ef03d6a8303747701e1c/server/tests-py/test_v1_queries.py#L565)

The following tests should also pass for MySQL sources:
```
# relationships
test_select_query_multiple_columns_arr_fkey
test_select_query_multiple_columns_obj_fkey
test_int_as_string_offset
test_err_neg_offset_error

# depend on relationships: nested queries
test_nested_select_query_article_author
test_nested_select_query_deep
```

## What

This feature translates to implementing certain instance methods for
MySQL sources, see https://github.com/hasura/graphql-engine-mono/pull/1110
as an example.

It is not currently expected that any changes would be necessary in the
common areas of the code, other than adding the appropriate API endpoints:
- `mysql_create_object_relationship`
- `mysql_create_array_relationship`
- `mysql_set_relationship_comment`
- `mysql_rename_relationship`
- `mysql_drop_relationship`

If a need for any non-trivial changes to the common code arises (such as,
but not limited to adding or changing method signatures in the `Backend*`
classes, IR code, parser code, etc.), this should be brought up with the
data sources and the IR teams to discuss options before proceeding.

On top of metadata-related operations, the following features are
implied when considering relationships:
- selecting data using relationships
- relationships in boolean expressions
- relationships in permission boolean expressions
- `_aggregate` fields for array relationships
- ordering using relationships

The above features are listed from most to least important.

### Selecting data using relationships

It is expected that creating a relationship creates a new field that
is selectable. For example, getting the author of a published article
by using`articles.author`.

```
query {
  articles {
    id
    title
	author {
	  id
	  name
	}
  }
}
```

Relevant test: `test_nested_select_query_article_author`

### Relationships in boolean expressions

Filtering should work on relationships, for example, we should be able
to filter articles by their author's name:

```
query {
  articles (where: {author: {name: {_eq: "Sidney"}}}) {
    id
    title
  }
}
```

Relevant test: `test_nested_select_query_where_on_relationship`

### Relationships in permission boolean expressions

Permissions should also be usable in boolean expressions. For more
details, please see the MySQL Permissions RFC.

### Aggregate fields for array relationshipsd

It is expected that each array relationship creates a `name_aggregate`
field:

```
query {
  autors {
    articles_aggregate {
	  aggregate {
	    count
	  }
	}
  }
}
```

Relevant test: `test_author_agg_with_articles`

### Ordering using relationships

Nested objects should be usable in ordering clauses.

```
query {
  articles( order_by: { author: { id: desc } } ) {
    id
	name
  }
}
```

Relevant test: `test_nested_select_query_where_on_relationship`

## Effects and Interactions

This feature will likely require support from the Console and CLI teams.
