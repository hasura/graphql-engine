# MSSQL Update Mutations

## Metadata

```
---
authors: Philip Carlsen <philip@hasura.io>
discussion:
  https://github.com/hasura/graphql-engine-mono/issues/3518
  https://github.com/hasura/graphql-engine-mono/issues/2746#issuecomment-1009977143
state: draft
---
```

## Description

This RFC describes how update mutations should work in MSSQL, highlighting the
ways they are different from the update mutations on Postgres.

Initially we just wanted to support update mutations on MSSQL in the same way
that we do on Postgres.

However, there are some differences between the two database systems that
warrent closer inspection.

### Problem

We want to be able to support manipulating JSON values via update mutations
as much as the database supports it.

Update mutations are expressed using "update operators", which indicate for each
column what function should apply be applied to obtain an updated value.
Textbook examples are `_set` which just sets the column to some constant, and
`_inc` which increments the (numeric) column value by some constant.

The above operators are fairly foundational and easily supported in all
conceivable relational databases. For JSON transformations however, Postgres and
MSSQL diverge: Postgres supplies a lot of [dedicated JSON manipulation
functions](https://www.postgresql.org/docs/current/functions-json.html) that we
can export directly, while MSSQL supplies only a [single JSON manipulation
primitive](https://docs.microsoft.com/en-us/sql/t-sql/functions/json-modify-transact-sql?view=sql-server-ver15)
that has to be combined with `OPEN JSON` and `FOR JSON` clauses to do more
elaborate transformations.

### Costs and Drawbacks

Because Postgres has such a rich library of functions we can get by without
having to implement a proper expression language.

In MSSQL, because there is only a single function which may set/append/remove
the values of object fields, if you want to, say append two JSON lists you have
to first convert those to SQL tables via `SELECT .. OPEN JSON`, apply SQL
`UNION` on those tables, and rebuild the json list via a `FOR JSON` clause.

Supporting this however seems quite complicated, as it mixes SQL SELECT queries
with scalar expressions.

A much more sensible approach is to first of all just support the scalar
function `JSON_MODIFY` and potentially the other expression-level json
functions.

### Future Work / Out of Scope

If there turns out to be a demand for more elaborate JSON transformations we can
revisit this. A powerful way to address this would be to enable users to track
expression-level functions in the GraphQL Engine and let them use those in
update mutations. This would make our solution simpler, and more maintainable
and featureful at the same time, since it is not a novel development task
whenever users need new update operators (and boolean operators are potential
candidates for this as well).

## What

Our schema for update mutations on MSSQL are extended with one more update
operator for applying JSON transformations.

We have two possible approaches:

### Support only JSON-valued constants

The simplest solution (which is also the least powerful) is to just support a
single top-level call to `JSON_MODIFY` which may only be passed constant values:

```graphql

type <table>_update {
  ...
  _json_modify: <table>_json_modify_columns
}
  
input <table>_json_modify_columns {
  """
  Columns that should be updated via a `JSON_MODIFY` expression
  """

  <text columns of table>: <table>_json_modify_inputs
}

input <table>_json_modify_inputs {
  """
  Arguments to `JSON_MODIFY`.
  """
  path: mssql_json_path!
  value: mssql_json
}
  
"""A JSON PATH string, of the format: ( 'strict ' | 'lax ' )? '$' <path with dots and brackets>"""
scalar mssql_json_path

"""Literal JSON values, either passed as variables or inlined as Strings."""
scalar mssql_json

enum <table>_select_column {
  """All columns that the current role have select permissions for"""
  ...
}
```

This will only let a user set a field somewhere in a JSON object structure to a
new constant value, or remove the field.

A non-exhaustive list of notable things you *cannot* do with this approach include:
* append lists onto fields
* increment numeric fields
* rename fields
* most of the
  [examples](https://docs.microsoft.com/en-us/sql/t-sql/functions/json-modify-transact-sql?view=sql-server-ver15#examples)
  listed in the `JSON_MODIFY` documentation.

*Usage examples:*

Update an object field to a constant value:

```graphql
update_table(
  where: {},
  _json_modify:
  {
    data:
    {
      path: "lax $.name",
      new_value: "John Doe"
    }
  }
  )
  { affected_rows }
```

This would roughly translate into SQL as:
```SQL
UPDATE ... SET data = JSON_MODIFY(data, 'lax $.name', 'John Doe')
```

### Support richer JSON-valued expressions

Alternatively we can define a grammar for expressions that produce a json value,
and permit its use in the value field of the `json_modify` object:

```graphql
type <table>_update {
  ...
  _json: <table>_json_expression_columns
}
  
input <table>_json_expression_columns {
  """
  Columns that should be updated via a JSON-valued expression
  """

  <text columns of table>: <table>_json_expression
}

input <table>_json_expression {
  """
  This object encodes the ways to build a JSON-valued expression.
  Only a single field may be specified. You can think of this as an input union type.
  """

  column: <table>_select_column
  constant: String
  json_literal: json
  json_value: <table>_json_value_inputs
  json_query: <table>_json_query_inputs
  json_modify: <table>_json_modify_inputs
}

input <table>_json_value_inputs {
  """
  Arguments to `JSON_VALUE`. The `path` field must reference a scalar value.
  """
  path: mssql_json_path!
  expression: <table>_json_expression
}

input <table>_json_query_inputs {
  """
  Arguments to `JSON_QUERY`. The `path` field must reference an object or array value.
  """
  expression: <table>_json_expression
  path: mssql_json_path!
}

input <table>_json_modify_inputs {
  """
  Arguments to `JSON_MODIFY`.
  """
  expression: <table>_json_expression
  path: mssql_json_path!
  new_value: <table>_json_expression
}
  
"""A JSON PATH string, of the format: ( 'strict ' | 'lax ' )? '$' <path with dots and brackets>  """
scalar mssql_json_path

"""Literal JSON objects, either passed as variables or inlined as Strings."""
scalar mssql_json

enum <table>_select_column {
  """All columns of <table> that the current role have select permissions for"""
  ...
}
```

This formulation lets users nest calls to `JSON_MODIFY`, `JSON_VALUE`, and
`JSON_QUERY` arbitrarily, and permits referencing other columns of the same
table in addition to providing constants.

This permits all the examples listed in the `JSON_MODIFY` documentation.

But it does not let us go all the way: Appending lists or merging objects are
still not expressible, because we stay within the realm of scalar functions.
Also, only the columns of the current table are in scope, and we cannot reference
e.g. columns from parent tables in relationships.

*Usage examples:*

Update an object field to a constant value:

```graphql
update_table(
  where: {},
  _json:
  {
    data:
    {
      json_modify
      {
        expression: {column: data},
        path: "lax $.name",
        new_value: {constant: "John Doe"}
      }
    }
  }
  )
  { affected_rows }
```
This would roughly translate into SQL as:
```SQL
UPDATE ... SET data = JSON_MODIFY(data, 'lax $.name', 'John Doe')
```

(Note that the expression is necessarily more elaborate than the same example in proposal 1)

Rename a field:

```graphql
update_table(
  where: {},
  _json:
  {
    data:
    {
      json_modify # Erase field $.old
      {
        path: "lax $.old",
        new_value: {json_literal: "null"}
        expression:
        {
          json_modify: # set $.new = $.old
          {
            expression: {column: data},
            path: "lax $.new",
            new_value:
            {
              json_query:
              {
                path: "lax $.old"
                expression: {column: data}
              }
            }
          }
        }
      }
    }
  }
  )
  { affected_rows }
```
This would roughly translate into SQL as:
```SQL
UPDATE ... SET data = JSON_MODIFY(JSON_MODIFY(data, 'lax $.new', JSON_QUERY(data, 'lax $.old')), 'lax $.old', JSON_VALUE('null'))
```

### Which to choose?

The first proposal is certainly the simplest to implement.

However, the second solution is more powerful. Also, since the proposed GraphQL
schema is congruent with the SQL scalar expressions we are going to generate,
the translation seems straightforward too. The only complexity I see is in
referencing other columns, as that requires keeping track of aliases.

The second proposal is also more naturally extensible with new (potentially
user-defined) operations, whereas the first proposal is not. It is expressive
enough to subsume the existing approach to update operators as well.

## How

The implementation will need to:

* Extend the set of MSSQL update operators
  `Hasura.Backends.MSSQL.Update.Types.UpdateOperator` with a case for
  `json_modify`, which will hold the `json_modify_inputs` values.
* Define the schema for the new update operator as a
  `Hasura.GraphQL.Schema.Update.UpdateOperator` and add it to the list of update
  operators enabled in `Hasura.Backends.MSSQL.Instances.Schema`.
* Extend the SQL translation in `Hasura.Backends.MSSQL.Execute.Update` to handle
  the new update operator.

### Effects and Interactions

This is purely a change to the GraphQL schema - No changes need to happen
anywhere but the server.

### Unresolved Questions

Should we select the first or the second proposal?
