# Data Connectors

This document describes the current specification of the new _data connectors_s feature of `graphql-engine`, which is under active development.

The data connectors feature allows `graphql-engine` to delegate the execution of operations to external web services called _agents_. Such agents provide access to a data set, allowing `graphql-engine` to query that data set over a web API.

This document specifies (1) the web API that must be presented by agents, and (2) the precise behaviour of agents for specific reference data sets.

For further reference, the directory in which this document resides contains some implementations of different agents:
- [Reference Implementation](./reference/) - A reference implementation in TypeScript that serves some static in-memory data

## Stability

This specification is complete with regards to the current implementation, but should be considered _unstable_ until the Data Connectors feature is officially released and explicitly marked as a non-experimental feature.

## Setting up Data Connector agents with `graphql-engine`

In order to run one of the example agents, follow the steps in its respective README document.

Once an agent is running, import the following metadata into `graphql-engine`:

```json
POST /v1/metadata

{
  "type": "replace_metadata",
  "args": {
    "metadata": {
      "version": 3,
      "backend_configs": {
        "dataconnector": {
          "reference": {
            "uri": "http://localhost:8100/"
          }
        }
      },
      "sources": [
        {
          "name": "chinook",
          "kind": "reference",
          "tables": [
            {
              "table": ["Album"],
              "object_relationships": [
                {
                  "name": "Artist",
                  "using": {
                    "manual_configuration": {
                      "remote_table": ["Artist"],
                      "column_mapping": {
                        "ArtistId": "ArtistId"
                      }
                    }
                  }
                }
              ]
            },
            {
              "table": ["Artist"],
              "array_relationships": [
                {
                  "name": "Album",
                  "using": {
                    "manual_configuration": {
                      "remote_table": ["Album"],
                      "column_mapping": {
                        "ArtistId": "ArtistId"
                      }
                    }
                  }
                }
              ]
            }
          ],
          "configuration": {
            "value": {
              "tables": [ "Artist", "Album" ]
            }
          }
        }
      ]
    }
  }
}
```

The `backend_configs.dataconnector` section lets you set the URIs for as many agents as you'd like. In this case, we've defined one called "reference". When you create a source, the `kind` of the source should be set to the name you gave the agent in the `backend_configs.dataconnector` section (in this case, "reference").

The `configuration` property under the source can contain an 'arbitrary' JSON object, and this JSON will be sent to the agent on every request via the `X-Hasura-DataConnector-Config` header. The example here is configuration that the reference agent uses. The JSON object must conform to the schema specified by the agent from its `/capabilities` endpoint.

The `name` property under the source will be sent to the agent on every request via the `X-Hasura-DataConnector-SourceName` header. This name uniquely identifies a source within an instance of HGE.

The `albums` and `artists` tables should now be available in the GraphiQL console. You should be able to issue queries via the web service. For example:

```graphql
query {
  artists {
    name
    albums {
      title
    }
  }
}
```

## Implementing Data Connector agents

This section is a guide to implementing Data Connector agents for `graphql-engine`. You may find it useful to consult the code examples for reference.

The entry point to the reference agent application is a Fastify HTTP server. Raw data is loaded from JSON files on disk, and the server provides the following endpoints:

- `GET /capabilities`, which returns the capabilities of the agent and a schema that describes the type of the configuration expected to be sent on the `X-Hasura-DataConnector-Config` header
- `GET /schema`, which returns information about the provided _data schema_, its tables and their columns
- `POST /query`, which receives a query structure to be executed, encoded as the JSON request body, and returns JSON conforming to the schema described by the `/schema` endpoint, and contining the requested fields.
- `GET /health`, which can be used to either check if the agent is running, or if a particular data source is healthy

The `/schema` and `/query` endpoints require the request to have the `X-Hasura-DataConnector-Config` header set. That header contains configuration information that agent can use to configure itself. For example, the header could contain a connection string to the database, if the agent requires a connection string to know how to connect to a specific database. The header must be a JSON object, but the specific properties that are required are up to the agent to define.

The `/schema` and `/query` endpoints also require the request to have the `X-Hasura-DataConnector-SourceName` header set. This header contains the name of the data source configured in HGE that will be querying the agent. This can be used by the agent to maintain things like connection pools and configuration maps on a per-source basis.

We'll look at the implementation of each of the endpoints in turn.

### Capabilities and configuration schema

The `GET /capabilities` endpoint is used by `graphql-engine` to discover the capabilities supported by the agent, and so that it can know the correct shape of configuration data that needs to be collected from the user and sent to the agent in the `X-Hasura-DataConnector-Config` header. It should return a JSON object similar to the following:

```json
{
  "capabilities": {
    "relationships": {},
    "graphql_schema": "scalar DateTime\n\ninput DateTimeComparisons {\n  in_year: Number\n}",
    "scalar_types": {
      "DateTime": {"comparisonType": "DateTimeComparisons"}
    }
  },
  "config_schemas": {
    "config_schema": {
      "type": "object",
      "nullable": false,
      "properties": {
        "tables": { "$ref": "#/other_schemas/Tables" }
      }
    },
    "other_schemas": {
      "Tables": {
        "description": "List of tables to make available in the schema and for querying",
        "type": "array",
        "items": { "$ref": "#/other_schemas/TableName" },
        "nullable": true
      },
      "TableName": {
        "nullable": false,
        "type": "string"
      }
    }
  }
}
```

The `capabilities` section describes the _capabilities_ of the service. This includes
- `relationships`: whether or not the agent supports relationships
- `scalar_types`: custom scalar types and the operations they support. See [Scalar types capabilities](#scalar-type-capabilities).
- `graphql_schema`: a GraphQL schema document containing type definitions referenced by the `scalar_types` capabilities.

The `config_schema` property contains an [OpenAPI 3 Schema](https://swagger.io/specification/#schema-object) object that represents the schema of the configuration object. It can use references (`$ref`) to refer to other schemas defined in the `other_schemas` object by name.

`graphql-engine` will use the `config_schema` OpenAPI 3 Schema to validate the user's configuration JSON before putting it into the `X-Hasura-DataConnector-Config` header.

#### Scalar type capabilities

The agent is expected to support a default set of scalar types (`Number`, `String`, `Bool`) and a default set of [comparison operators](#filters) on these types.
Agents may optionally declare support for their own custom scalar types and custom comparison operators on those types.
Hasura GraphQL Engine does not validate the JSON format for values of custom scalar types.
It passes them through transparently to the agent when they are used as GraphQL input values and returns them transparently when they are produced by the agent.
It is the agent's responsibility to validate the values provided as GraphQL inputs.

Custom scalar types are declared by adding a property to the `scalar_types` section of the [capabilities](#capabilities-and-configuration-schema) and
by adding scalar type declaration with the same name in the `graphql_schema` capabilities property.
Custom comparison types can be defined by adding a `comparisonType` property to the scalar type capabilities object.
The `comparisonType` property gives the name of a GraphQL input object type, which must be defined in the `graphql_schema` capabilities property.
The input object type will be spliced into the `where` argument for any columns of the scalar type in the GraphQL schema.

Example:

```yaml
capabilities:
  graphql_schema: |
    scalar DateTime

    input DateTimeComparisons {
      in_year: Number
    }
  scalar_types:
    DateTime:
      comparisonType: DateTimeComparisons
```

This example declares a custom scalar type `DateTime`, with comparison operators defined by the GraphQL input object type `DateTimeComparisons`.
The input type `DateTimeComparisons` defines one comparison operator `in_year` which takes a `Number` argument

An example GraphQL query using this custom operator might look like below:
```graphql
query MyQuery {
  Employee(where: {BirthDate: {in_year: 1962}}) {
    Name
    BirthDate
  }
}
```
In this query we have an `Employee` field with a `BirthDate` property of type `DateTime`.
The `in_year` custom comparison operator is being used to request all employees with a birth date in the year 1962.

### Schema

The `GET /schema` endpoint is called whenever the metadata is (re)loaded by `graphql-engine`. It returns the following JSON object:

```json
{
  "tables": [
    {
      "name": ["Artist"],
      "primary_key": ["ArtistId"],
      "description": "Collection of artists of music",
      "columns": [
        {
          "name": "ArtistId",
          "type": "number",
          "nullable": false,
          "description": "Artist primary key identifier"
        },
        {
          "name": "Name",
          "type": "string",
          "nullable": true,
          "description": "The name of the artist"
        }
      ]
    },
    {
      "name": ["Album"],
      "primary_key": ["AlbumId"],
      "description": "Collection of music albums created by artists",
      "columns": [
        {
          "name": "AlbumId",
          "type": "number",
          "nullable": false,
          "description": "Album primary key identifier"
        },
        {
          "name": "Title",
          "type": "string",
          "nullable": false,
          "description": "The title of the album"
        },
        {
          "name": "ArtistId",
          "type": "number",
          "nullable": false,
          "description": "The ID of the artist that created this album"
        }
      ]
    }
  ]
}
```

The `tables` section describes the two available tables, as well as their columns, including types and nullability information.

Notice that the names of tables and columns are used in the metadata document to describe tracked tables and relationships.

Table names are described as an array of strings. This allows agents to fully qualify their table names with whatever namespacing requirements they have. For example, if the agent connects to a database that puts tables inside schemas, the agent could use table names such as `["my_schema", "my_table"]`.

#### Type definitions

The `SchemaResponse` TypeScript type from [the reference implementation](./reference/src/types/index.ts) describes the valid response body for the `GET /schema` endpoint.

### Responding to queries

The `POST /query` endpoint is invoked when the user requests data from `graphql-engine` which is resolved by the service.

The service logs queries from the request body in the console. Here is a simple example based on a GraphQL query which fetches all artist data:

```graphql
query {
  Artist {
    ArtistId
    Name
  }
}
```

and here is the resulting query request payload:

```json
{
  "table": ["Artist"],
  "table_relationships": [],
  "query": {
    "where": {
      "expressions": [],
      "type": "and"
    },
    "order_by": null,
    "limit": null,
    "offset": null,
    "fields": {
      "ArtistId": {
        "type": "column",
        "column": "ArtistId"
      },
      "Name": {
        "type": "column",
        "column": "Name"
      }
    }
  }
}
```

The implementation of the service is responsible for intepreting this data structure and producing a JSON response body which is compatible with both the query and the schema.

Let's break down the request:

- The `table` field tells us which table to fetch the data from, namely the `Artist` table. The table name (ie. the array of strings) must be one that was returned previously by the `/schema` endpoint.
- The `table_relationships` field that lists any relationships used to join between tables in the query. This query does not use any relationships, so this is just an empty list here.
- The `query` field contains further information about how to query the specified table:
  - The `where` field tells us that there is currently no (interesting) predicate being applied to the rows of the data set (just an empty conjunction, which ought to return every row).
  - The `order_by` field tells us that there is no particular ordering to use, and that we can return data in its natural order.
  - The `limit` and `offset` fields tell us that there is no pagination required.
  - The `fields` field tells us that we ought to return two fields per row (`ArtistId` and `Name`), and that these fields should be fetched from the columns with the same names.

#### Response Body Structure

The response body for a call to `POST /query` must conform to a specific query response format. Here's an example:

```json
{
  "rows": [
    {
      "ArtistId": 1,
      "Name": "AC/DC"
    },
    {
      "ArtistId": 2,
      "Name": "Accept"
    }
  ]
}
```

The rows returned by the query must be put into the `rows` property array in the query response object. Each object within this array represents a row, and the row object properties are the fields requested in the query. The value of the row object properties can be one of two types:

- `column`: The field was a column field, then value of that column for this row is used
- `relationship`: If the field was a relationship field, then a new query response object that contains the results of navigating that relationship for the current row must be used. (The query response structure is recursive via relationship-typed field values). Examples of this can be seen in the Relationships section below.

#### Pagination

If the GraphQL query contains pagination information, then the `limit` and `offset` fields may be set to integer values, indicating the number of rows to return, and the index of the first row to return, respectively.

#### Filters

The `where` field contains a recursive expression data structure which should be interpreted as a predicate in the context of each record.

Each node of this recursive expression structure is tagged with a `type` property, which indicates the type of that node, and the node will contain one or more additional fields depending on that type. The valid expression types are enumerated below, along with these additional fields:

| type            | Additional fields              | Description |
|-----------------|--------------------------------|-------------|
| `and`           | `expressions`                  | A conjunction of several subexpressions |
| `or`            | `expressions`                  | A disjunction of several subexpressions |
| `not`           | `expression`                   | The negation of a single subexpression |
| `exists`        | `in_table`, `where`            | Test if a row exists that matches the `where` subexpression in the specified table (`in_table`) |
| `binary_op`     | `operator`, `column`, `value`  | Test the specified `column` against a single `value` using a particular binary comparison `operator` |
| `binary_arr_op` | `operator`, `column`, `values` | Test the specified `column` against an array of `values` using a particular binary comparison `operator` |
| `unary_op`      | `operator`, `column`           | Test the specified `column` against a particular unary comparison `operator` |

The value of the `in_table` property of the `exists` expression is an object that describes which table to look for rows in. The object is tagged with a `type` property:

| type        | Additional fields | Description |
|-------------|---------------------------------|
| `related`   | `relationship`    | The table is related to the current table via the relationship name specified in `relationship` (this means it should be joined to the current table via the relationship) |
| `unrelated` | `table`           | The table specified by `table` is unrelated to the current table and therefore is not explicitly joined to the current table |

The "current table" during expression evaluation is the table specified by the closest ancestor `exists` expression, or if there is no `exists` ancestor, it is the table involved in the Query that the whole `where` Expression is from.

The available binary comparison operators that can be used against a single value in `binary_op` are:

| Binary comparison operator | Description |
|----------------------------|-------------|
| `less_than`                | The `<` operator |
| `less_than_or_equal`       | The `<=` operator |
| `greater_than`             | The `>` operator |
| `greater_than_or_equal`    | The `>=` operator |
| `equal`                    | The `=` operator |

The available binary comparison operators that can be used against an array of values in `binary_arr_op` are:

| Binary array comparison operator | Description |
|----------------------------------|-------------|
| `in`                             | The SQL `IN` operator (ie. the column must be any of the array of specified values) |

The available unary comparison operators that can be used against a column:

| Unary comparison operator | Description |
|---------------------------|-------------|
| `is_null`                 | Tests if a column is null |

Values (as used in `value` in `binary_op` and the `values` array in `binary_arr_op`) are specified as either a literal value, or a reference to another column, which could potentially be in another related table in the same query. The value object is tagged with a `type` property and has different fields based on the type.

| type     | Additional fields | Description |
|----------|-------------------|-------------|
| `scalar` | `value`           | A scalar `value` to compare against |
| `column` | `column`          | A `column` in the current table being queried to compare against |

Columns (as used in `column` fields in `binary_op`, `binary_arr_op`, `unary_op` and in `column`-typed Values) are specified as a column `name`, as well as optionally a `path` to the table that contains the column. If the `path` property is missing/null or an empty array, then the column is on the current table. However, if the path is `["$"]`, then the column is on the table involved in the Query that the whole `where` expression is from. At this point in time, these are the only valid values of `path`.

Here is a simple example, which correponds to the predicate "`first_name` is John and `last_name` is Smith":

```json
{
  "type": "and",
  "expressions": [
    {
      "type": "binary_op",
      "operator": "equal",
      "column": {
        "name": "first_name"
      },
      "value": {
        "type": "scalar",
        "value": "John"
      }
    },
    {
      "type": "binary_op",
      "operator": "equal",
      "column": {
        "name": "last_name"
      },
      "value": {
        "type": "scalar",
        "value": "John"
      }
    }
  ]
}
```

Here's another example, which corresponds to the predicate "`first_name` is the same as `last_name`":

```json
{
  "type": "binary_op",
  "operator": "equal",
  "column": {
    "name": "first_name"
  },
  "value": {
    "type": "column",
    "column": {
      "name": "last_name"
    }
  }
}
```

In this example, a person table is filtered by whether or not that person has any children 18 years of age or older:

```json
{
  "type": "exists",
  "in_table": {
    "type": "related",
    "relationship": "children"
  },
  "where": {
    "type": "binary_op",
    "operator": "greater_than_or_equal",
    "column": {
      "name": "age"
    },
    "value": {
      "type": "scalar",
      "value": 18
    }
  }
}
```

In this example, a person table is filtered by whether or not that person has any children that have the same first name as them:

```jsonc
{
  "type": "exists",
  "in_table": {
    "type": "related",
    "relationship": "children"
  },
  "where": {
    "type": "binary_op",
    "operator": "equal",
    "column": {
      "name": "first_name" // This column refers to the child's name
    },
    "value": {
      "type": "column",
      "column": {
        "path": ["$"],
        "name": "first_name" // This column refers to the parent's name
      }
    }
  }
}
```

Exists expressions can be nested, but the `["$"]` path always refers to the query table. So in this example, a person table is filtered by whether or not that person has any children that have any friends that have the same first name as the parent:

```jsonc
{
  "type": "exists",
  "in_table": {
    "type": "related",
    "relationship": "children"
  },
  "where": {
    "type": "exists",
    "in_table": {
      "type": "related",
      "relationship": "friends"
    },
    "where": {
      "type": "binary_op",
      "operator": "equal",
      "column": {
        "name": "first_name" // This column refers to the children's friend's name
      },
      "value": {
        "type": "column",
        "column": {
          "path": ["$"],
          "name": "first_name" // This column refers to the parent's name
        }
      }
    }
  }
}
```

In this example, a table is filtered by whether or not an unrelated administrators table contains an admin called "superuser". Note that this means if the administrators table contains the "superuser" admin, then all rows of the table are returned, but if not, no rows are returned.

```json
{
  "type": "exists",
  "in_table": {
    "type": "unrelated",
    "table": ["administrators"]
  },
  "where": {
    "type": "binary_op",
    "operator": "equal",
    "column": {
      "name": "username"
    },
    "value": {
      "type": "scalar",
      "value": "superuser"
    }
  }
}
```

#### Relationships

If the call to `GET /capabilities` returns a `capabilities` record with a `relationships` field then the query structure may include fields corresponding to relationships.

_Note_ : if the `relationships` capability is not present then `graphql-engine` will not send queries to this agent involving relationships.

Relationship fields are indicated by a `type` field containing the string `relationship`. Such fields will also include the name of the relationship in a field called `relationship`. This name refers to a relationship that is specified on the top-level query request object in the `table_relationships` field.

This `table_relationships` is a list of tables, and for each table, a map of relationship name to relationship information. The information is an object that has a field `target_table` that specifies the name of the related table. It has a field called `relationship_type` that specified either an `object` (many to one) or an `array` (one to many) relationship. There is also a `column_mapping` field that indicates the mapping from columns in the source table to columns in the related table.

It is intended that the backend should execute the `query` contained in the relationship field and return the resulting query response as the value of this field, with the additional record-level predicate that any mapped columns should be equal in the context of the current record of the current table.

An example will illustrate this. Consider the following GraphQL query:

```graphql
query {
  Artist {
    Name
    Albums {
      Title
    }
  }
}
```

This will generate the following JSON query if the agent supports relationships:

```json
{
  "table": ["Artist"],
  "table_relationships": [
    {
      "source_table": ["Artist"],
      "relationships": {
        "ArtistAlbums": {
          "target_table": ["Album"],
          "relationship_type": "array",
          "column_mapping": {
            "ArtistId": "ArtistId"
          }
        }
      }
    }
  ],
  "query": {
    "where": {
      "expressions": [],
      "type": "and"
    },
    "offset": null,
    "order_by": null,
    "limit": null,
    "fields": {
      "Albums": {
        "type": "relationship",
        "relationship": "ArtistAlbums",
        "query": {
          "where": {
            "expressions": [],
            "type": "and"
          },
          "offset": null,
          "order_by": null,
          "limit": null,
          "fields": {
            "Title": {
              "type": "column",
              "column": "Title"
            }
          }
        }
      },
      "Name": {
        "type": "column",
        "column": "Name"
      }
    }
  }
}
```

Note the `Albums` field in particular, which traverses the `Artists` -> `Albums` relationship, via the `ArtistAlbums` relationship:

```json
{
  "type": "relationship",
  "relationship": "ArtistAlbums",
  "query": {
    "where": {
      "expressions": [],
      "type": "and"
    },
    "offset": null,
    "order_by": null,
    "limit": null,
    "fields": {
      "Title": {
        "type": "column",
        "column": "Title"
      }
    }
  }
}
```

The top-level `table_relationships` can be looked up by starting from the source table (in this case `Artist`), locating the `ArtistAlbums` relationship under that table, then extracting the relationship information. This information includes the `target_table` field which indicates the table to be queried when following this relationship is the `Album` table. The `relationship_type` field indicates that this relationship is an `array` relationship (ie. that it will return zero to many Album rows per Artist row). The `column_mapping` field indicates the column mapping for this relationship, namely that the Artist's `ArtistId` must equal the Album's `ArtistId`.

Back on the relationship field inside the query, there is another `query` field. This indicates the query that should be executed against the `Album` table, but we must remember to enforce the additional constraint between Artist's `ArtistId` and Album's `ArtistId`. That is, in the context of any single outer `Artist` record, we should populate the `Albums` field with the query response containing the array of Album records for which the `ArtistId` field is equal to the outer record's `ArtistId` field.

Here's an example (truncated) response:

```jsonc
{
  "rows": [
    {
      "Albums": {
        "rows": [
          {
            "Title": "For Those About To Rock We Salute You"
          },
          {
            "Title": "Let There Be Rock"
          }
        ]
      },
      "Name": "AC/DC"
    },
    {
      "Albums": {
        "rows": [
          {
            "Title": "Balls to the Wall"
          },
          {
            "Title": "Restless and Wild"
          }
        ]
      },
      "Name": "Accept"
    }
    // Truncated, more Artist rows here
  ]
}
```

#### Cross-Table Filtering
It is possible to form queries that filter their results by comparing columns across tables via relationships. One way this can happen in Hasura GraphQL Engine is when configuring permissions on a table. It is possible to configure a filter on a table such that it joins to another table in order to compare some data in the filter expression.

The following metadata when used with HGE configures a `Customer` and `Employee` table, and sets up a select permission rule on `Customer` such that only customers that live in the same country as their SupportRep Employee would be visible to users in the `user` role:

```json
POST /v1/metadata

{
  "type": "replace_metadata",
  "args": {
    "metadata": {
      "version": 3,
      "backend_configs": {
        "dataconnector": {
          "reference": {
            "uri": "http://localhost:8100/"
          }
        }
      },
      "sources": [
        {
          "name": "chinook",
          "kind": "reference",
          "tables": [
            {
              "table": ["Customer"],
              "object_relationships": [
                {
                  "name": "SupportRep",
                  "using": {
                    "manual_configuration": {
                      "remote_table": ["Employee"],
                      "column_mapping": {
                        "SupportRepId": "EmployeeId"
                      }
                    }
                  }
                }
              ],
              "select_permissions": [
                {
                  "role": "user",
                  "permission": {
                    "columns": [
                      "CustomerId",
                      "FirstName",
                      "LastName",
                      "Country",
                      "SupportRepId"
                    ],
                    "filter": {
                      "SupportRep": {
                        "Country": {
                          "_ceq": ["$","Country"]
                        }
                      }
                    }
                  }
                }
              ]
            },
            {
              "table": ["Employee"]
            }
          ],
          "configuration": {}
        }
      ]
    }
  }
}
```

Given this GraphQL query (where the `X-Hasura-Role` session variable is set to `user`):

```graphql
query getCustomer {
  Customer {
    CustomerId
    FirstName
    LastName
    Country
    SupportRepId
  }
}
```

We would get the following query request JSON:

```json
{
  "table": ["Customer"],
  "table_relationships": [
    {
      "source_table": ["Customer"],
      "relationships": {
        "SupportRep": {
          "target_table": ["Employee"],
          "relationship_type": "object",
          "column_mapping": {
            "SupportRepId": "EmployeeId"
          }
        }
      }
    }
  ],
  "query": {
    "fields": {
      "Country": {
        "type": "column",
        "column": "Country"
      },
      "CustomerId": {
        "type": "column",
        "column": "CustomerId"
      },
      "FirstName": {
        "type": "column",
        "column": "FirstName"
      },
      "LastName": {
        "type": "column",
        "column": "LastName"
      },
      "SupportRepId": {
        "type": "column",
        "column": "SupportRepId"
      }
    },
    "where": {
      "type": "and",
      "expressions": [
        {
          "type": "exists",
          "in_table": {
            "type": "related",
            "relationship": "SupportRep"
          },
          "where": {
            "type": "binary_op",
            "operator": "equal",
            "column": {
              "name": "Country"
            },
            "value": {
              "type": "column",
              "column": {
                "path": ["$"],
                "name": "Country"
              }
            }
          }
        }
      ]
    }
  }
}
```

The key point of interest here is in the `where` field where we are comparing between columns. Our first expression is an `exists` expression that specifies a row must exist in the table related to the `Customer` table by the `SupportRep` relationship (ie. the `Employee` table). These rows must match a subexpression that compares the related `Employee`'s `Country` column with `equal` to `Customer`'s `Country` column (as indicated by the `["$"]` path). So, in order to evaluate this condition, we'd need to join the `Employee` table using the `column_mapping` specified in the `SupportRep` relationship. Then if any of the related rows (in this case, only one because it is an `object` relation) contain a `Country` that is equal to Customer row's `Country` the `binary_op` would evaluate to True. This would mean a row exists, so the `exists` evaluates to true, and we don't filter out the Customer row.

#### Filtering by Unrelated Tables
It is possible to filter a table by a predicate evaluated against a completely unrelated table. This can happen in Hasura GraphQL Engine when configuring permissions on a table.

In the following example, we are configuring HGE's metadata such that when the Customer table is queried by the employee role, the employee currently doing the query (as specified by the `X-Hasura-EmployeeId` session variable) must be an employee from the city of Calgary, otherwise no rows are returned.

```json
POST /v1/metadata

{
  "type": "replace_metadata",
  "args": {
    "metadata": {
      "version": 3,
      "backend_configs": {
        "dataconnector": {
          "reference": {
            "uri": "http://localhost:8100/"
          }
        }
      },
      "sources": [
        {
          "name": "chinook",
          "kind": "reference",
          "tables": [
            {
              "table": ["Customer"],
              "select_permissions": [
                {
                  "role": "employee",
                  "permission": {
                    "columns": [
                      "CustomerId",
                      "FirstName",
                      "LastName",
                      "Country",
                      "SupportRepId"
                    ],
                    "filter": {
                      "_exists": {
                        "_table": ["Employee"],
                        "_where": {
                          "_and": [
                            { "EmployeeId": { "_eq": "X-Hasura-EmployeeId" } },
                            { "City": { "_eq": "Calgary" } }
                          ]
                        }
                      }
                    }
                  }
                }
              ]
            },
            {
              "table": ["Employee"]
            }
          ],
          "configuration": {}
        }
      ]
    }
  }
}
```

Given this GraphQL query (where the `X-Hasura-Role` session variable is set to `employee`, and the `X-Hasura-EmployeeId` session variable is set to `2`):

```graphql
query getCustomer {
  Customer {
    CustomerId
    FirstName
    LastName
    Country
    SupportRepId
  }
}
```

We would get the following query request JSON:

```json
{
  "table": ["Customer"],
  "table_relationships": [],
  "query": {
    "fields": {
      "Country": {
        "type": "column",
        "column": "Country"
      },
      "CustomerId": {
        "type": "column",
        "column": "CustomerId"
      },
      "FirstName": {
        "type": "column",
        "column": "FirstName"
      },
      "LastName": {
        "type": "column",
        "column": "LastName"
      },
      "SupportRepId": {
        "type": "column",
        "column": "SupportRepId"
      }
    },
    "where": {
      "type": "exists",
      "in_table": {
        "type": "unrelated",
        "table": ["Employee"]
      },
      "where": {
        "type": "and",
        "expressions": [
          {
            "type": "binary_op",
            "operator": "equal",
            "column": {
              "name": "EmployeeId"
            },
            "value": {
              "type": "scalar",
              "value": 2
            }
          },
          {
            "type": "binary_op",
            "operator": "equal",
            "column": {
              "name": "City"
            },
            "value": {
              "type": "scalar",
              "value": "Calgary"
            }
          }
        ]
      }
    }
  }
}
```

The key part in this query is the `where` expression. The root expression in the where is an `exists` expression which specifies that at least one row must exist in the unrelated `["Employee"]` table that satisfies a subexpression. This subexpression asserts that the rows from the Employee table have both `EmployeeId` as `2` and `City` as `Calgary`. The columns referenced inside this subexpression don't have `path` properties, which means they refer the columns on the Employee table because that is the closest ancestor `exists` table.

#### Aggregates
HGE supports forming GraphQL queries that allow clients to aggregate over the data in their data sources. This type of query can be passed through to Data Connector agents as a part of the Query structure sent to `/query`.

For example, consider the following GraphQL query:

```graphql
query {
  Artist_aggregate {
    aggregate {
      max {
        ArtistId
      }
    }
  }
}
```

This would cause the following query request to be performed:

```json
{
  "table": ["Artist"],
  "table_relationships": [],
  "query": {
    "aggregates": {
      "aggregate_max_ArtistId": {
        "type": "single_column",
        "function": "max",
        "column": "ArtistId"
      }
    }
  }
}
```

Notice the `Query` has an `aggregates` property; this property contains an object where the property name is the field name of the aggregate, and the value is a description of the aggregate. In the example above, we're using the `max` function on the `ArtistId` column. The `max` function is a function that operates on a single column, so the type of the aggregate is `single_column`.

These are the supported `single_column` functions:
- `avg`
- `max`
- `min`
- `stddev_pop`
- `stddev_samp`
- `sum`
- `var_pop`
- `var_samp`

The aggregate function is to be run over all rows that match the `Query`. In this case, the query has no filters on it (ie. no `where`, `limit` or `offset` properties), so the query would be selecting all rows in the Artist table.

There are two other types of aggregates, `column_count` and `star_count`, as demonstrated in this GraphQL query, and its resultant `QueryRequest`:

```graphql
query {
  Album_aggregate {
    aggregate {
      distinct_count: count(columns: Title, distinct: true)
      count
    }
  }
}
```

```json
{
  "table": ["Album"],
  "table_relationships": [],
  "query": {
    "aggregates": {
      "aggregate_distinct_count": {
        "type": "column_count",
        "columns": ["Title"],
        "distinct": true
      },
      "aggregate_count": {
        "type": "star_count"
      }
    }
  }
}
```

A `column_count` aggregate counts the number of rows that have non-null values in the specified `columns`. If `distinct` is set to `true`, then the count should only count unique values of those columns. This is like a `COUNT(x,y,z)` or a `COUNT(DISTINCT x,y,z)` in SQL.

A `star_count` aggregate simply counts the number of rows matched by the query (similar to a `COUNT(*)` in SQL).

The results of the aggregate functions must be returned in an `aggregates` property on the query response. For example:

```json
{
  "aggregates": {
    "aggregate_distinct_count": 347,
    "aggregate_count": 347
  }
}
```

HGE's aggregate GraphQL queries can also return the rows involved in the aggregates, as well as apply all the standard filtering operations, for example:

```graphql
query {
  Artist_aggregate(where: {Name: {_gt: "Z"}}) {
    aggregate {
      count
    }
    nodes {
      ArtistId
      Name
    }
  }
}
```

The `nodes` part of the query ends up as standard `fields` in the `Query`, and therefore are treated exactly the same as discussed in previous sections:

```json
{
  "table": ["Artist"],
  "table_relationships": [],
  "query": {
    "aggregates": {
      "aggregate_count": {
        "type": "star_count"
      }
    },
    "fields": {
      "nodes_ArtistId": {
        "type": "column",
        "column": "ArtistId"
      },
      "nodes_Name": {
        "type": "column",
        "column": "Name"
      }
    },
    "where": {
      "type": "binary_op",
      "operator": "greater_than",
      "column": {
        "name": "Name"
      },
      "value": {
        "type": "scalar",
        "value": "Z"
      }
    }
  },
}
```

The response from this query would include both the `aggregates` and the matching `rows` containing the specified `fields`:

```json
{
  "aggregates": {
    "aggregate_count": 1
  },
  "rows": [
    {
      "nodes_ArtistId": 155,
      "nodes_Name": "Zeca Pagodinho"
    }
  ]
}
```

Aggregate queries can also appear in relationship fields. Consider the following query:

```graphql
query {
  Artist(limit: 2, offset: 1) {
    Name
    Albums_aggregate {
      aggregate {
        count
      }
    }
  }
}
```

This would generate the following `QueryRequest`:

```json
{
  "table": ["Artist"],
  "table_relationships": [
    {
      "source_table": ["Artist"],
      "relationships": {
        "Albums": {
          "target_table": ["Album"],
          "relationship_type": "array",
          "column_mapping": {
            "ArtistId": "ArtistId"
          }
        }
      }
    }
  ],
  "query": {
    "fields": {
      "Albums_aggregate": {
        "type": "relationship",
        "relationship": "Albums",
        "query": {
          "aggregates": {
            "aggregate_count": {
              "type": "star_count"
            }
          }
        }
      },
      "Name": {
        "type": "column",
        "column": "Name"
      }
    },
    "limit": 2,
    "offset": 1
  }
}
```

This would be expected to return the following response, with the rows from the Artist table, and the aggregates from the related Albums nested under the relationship field values for each Album row:

```json
{
  "rows": [
    {
      "Albums_aggregate": {
        "aggregates": {
          "aggregate_count": 2
        }
      },
      "Name": "Accept"
    },
    {
      "Albums_aggregate": {
        "aggregates": {
          "aggregate_count": 1
        }
      },
      "Name": "Aerosmith"
    }
  ]
}
```

#### Ordering

The `order_by` field can either be null, which means no particular ordering is required, or an object with two properties:

```json
{
  "relations": {},
  "elements": [
    {
      "target_path": [],
      "target": {
        "type": "column",
        "column": "last_name"
      },
      "order_direction": "asc"
    },
    {
      "target_path": [],
      "target": {
        "type": "column",
        "column": "first_name"
      },
      "order_direction": "desc"
    }
  ]
}
```

The `elements` field specifies an array of one-or-more ordering elements. Each element represents a "target" to order, and a direction to order by. The direction can either be `asc` (ascending) or `desc` (descending). If there are multiple elements specified, then rows should be ordered with earlier elements in the array taking precedence. In the above example, rows are principally ordered by `last_name`, delegating to `first_name` in the case where two last names are equal.

The order by element `target` is specified as an object, whose `type` property specifies a different sort of ordering target:

| type | Additional fields | Description |
|------|-------------------|-------------|
| `column` | `column` | Sort by the `column` specified |
| `star_count_aggregate` | - | Sort by the count of all rows on the related target table (a non-empty `target_path` will always be specified) |
| `single_column_aggregate` | `function`, `column` | Sort by the value of applying the specified aggregate function to the column values of the rows in the related target table (a non-empty `target_path` will always be specified) |

The `target_path` property is a list of relationships to navigate before finding the `target` to sort on. This is how sorting on columns or aggregates on related tables is expressed. Note that aggregate-typed targets will never be found on the current table (ie. a `target_path` of `[]`) and are always applied to a related table.

Here's an example of applying an ordering by a related table; the Album table is being queried and sorted by the Album's Artist's Name.

```json
{
  "table": ["Album"],
  "table_relationships": [
    {
      "source_table": ["Album"],
      "relationships": {
        "Artist": {
          "target_table": ["Artist"],
          "relationship_type": "object",
          "column_mapping": {
            "ArtistId": "ArtistId"
          }
        }
      }
    }
  ],
  "query": {
    "fields": {
      "Title": { "type": "column", "column": "Title" }
    },
    "order_by": {
      "relations": {
        "Artist": {
          "where": null,
          "subrelations": {}
        }
      },
      "elements": [
        {
          "target_path": ["Artist"],
          "target": {
            "type": "column",
            "column": "Name"
          },
          "order_direction": "desc"
        }
      ]
    }
  }
}
```

Note that the `target_path` specifies the relationship path of `["Artist"]`, and that this relationship is defined in the top-level `table_relationships`. The ordering element target column `Name` would therefore be found on the `Artist` table after joining to it from each `Album`. (See the [Relationships](#Relationships) section for more information about relationships.)

The `relations` property of `order_by` will contain all the relations used in the order by, for the purpose of specifying filters that must be applied to the joined tables before using them for sorting. The `relations` property captures all `target_path`s used in the `order_by` in a recursive fashion, so for example, if the following `target_path`s were used in the `order_by`'s `elements`:

* `["Artist", "Albums"]`
* `["Artist"]`
* `["Tracks"]`

Then the value of the `relations` property would look like this:

```json
{
  "Artist": {
    "where": null,
    "subrelations": {
      "Albums": {
        "where": null,
        "subrelations": {}
      }
    }
  },
  "Tracks": {
    "where": null,
    "subrelations": {}
  }
}
```

The `where` properties may contain filtering expressions that must be applied to the joined table before using it for sorting. The filtering expressions are defined in the same manner as specified in the [Filters](#Filters) section of this document, where they are used on the `where` property of Queries.

For example, here's a query that retrieves artists ordered descending by the count of all their albums where the album title is greater than 'T'.

```json
{
  "table": ["Artist"],
  "table_relationships": [
    {
      "source_table": ["Artist"],
      "relationships": {
        "Albums": {
          "target_table": ["Album"],
          "relationship_type": "array",
          "column_mapping": {
            "ArtistId": "ArtistId"
          }
        }
      }
    }
  ],
  "query": {
    "fields": {
      "Name": { "type": "column", "column": "Name" }
    },
    "order_by": {
      "relations": {
        "Albums": {
          "where": {
            "type": "binary_op",
            "operator": "greater_than",
            "column": {
              "name": "Title"
            },
            "value": {
              "type": "scalar",
              "value": "T"
            }
          },
          "subrelations": {}
        }
      },
      "elements": [
        {
          "target_path": ["Albums"],
          "target": {
            "type": "star_count_aggregate"
          },
          "order_direction": "desc"
        }
      ]
    }
  }
}
```

#### Type Definitions

The `QueryRequest` TypeScript type in the [reference implementation](./reference/src/types/index.ts) describes the valid request body payloads which may be passed to the `POST /query` endpoint. The response body structure is captured by the `QueryResponse` type.

### Health endpoint
Agents must expose a `/health` endpoint which must return a 204 No Content HTTP response code if the agent is up and running. This does not mean that the agent is able to connect to any data source it performs queries against, only that the agent is running and can accept requests, even if some of those requests might fail because a dependant service is unavailable.

However, this endpoint can also be used to check whether the ability of the agent to talk to a particular data source is healthy. If the endpoint is sent the `X-Hasura-DataConnector-Config` and `X-Hasura-DataConnector-SourceName` headers, then the agent is expected to check that it can successfully talk to whatever data source is being specified by those headers. If it can do so, then it must return a 204 No Content response code.
