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
              "table": "Album",
              "object_relationships": [
                {
                  "name": "Artist",
                  "using": {
                    "manual_configuration": {
                      "remote_table": "Artist",
                      "column_mapping": {
                        "ArtistId": "ArtistId"
                      }
                    }
                  }
                }
              ]
            },
            {
              "table": "Artist",
              "array_relationships": [
                {
                  "name": "Album",
                  "using": {
                    "manual_configuration": {
                      "remote_table": "Album",
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
            "tables": [ "Artist", "Album" ]
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
    "relationships": {}
  },
  "configSchemas": {
    "configSchema": {
      "type": "object",
      "nullable": false,
      "properties": {
        "tables": { "$ref": "#/otherSchemas/Tables" }
      }
    },
    "otherSchemas": {
      "Tables": {
        "description": "List of tables to make available in the schema and for querying",
        "type": "array",
        "items": { "$ref": "#/otherSchemas/TableName" },
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

The `capabilities` section describes the _capabilities_ of the service. Specifically, the service is capable of serving queries which involve relationships.

The `configSchema` property contains an [OpenAPI 3 Schema](https://swagger.io/specification/#schema-object) object that represents the schema of the configuration object. It can use references (`$ref`) to refer to other schemas defined in the `otherSchemas` object by name.

`graphql-engine` will use the `configSchema` OpenAPI 3 Schema to validate the user's configuration JSON before putting it into the `X-Hasura-DataConnector-Config` header.

### Schema and capabilities

The `GET /schema` endpoint is called whenever the metadata is (re)loaded by `graphql-engine`. It returns the following JSON object:

```json
{
  "tables": [
    {
      "name": "Artist",
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
      "name": "Album",
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
  "table": "Artist",
  "table_relationships": [],
  "query": {
    "where": {
      "expressions": [],
      "type": "and"
    },
    "order_by": [],
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

- The `table` field tells us which table to fetch the data from, namely the `Artist` table.
- The `table_relationships` field that lists any relationships used to join between tables in the query. This query does not use any relationships, so this is just an empty list here.
- The `query` field contains further information about how to query the specified table:
  - The `where` field tells us that there is currently no (interesting) predicate being applied to the rows of the data set (just an empty conjunction, which ought to return every row).
  - The `order_by` field tells us that there is no particular ordering to use, and that we can return data in its natural order.
  - The `limit` and `offset` fields tell us that there is no pagination required.
  - The `fields` field tells us that we ought to return two fields per row (`ArtistId` and `Name`), and that these fields should be fetched from the columns with the same names.

#### Response Body Structure

The response body for a call to `POST /query` should always consist of a JSON array of JSON objects. Each of those JSON objects should contain all of the same keys as the `fields` property of the request body, even if the values of the corresponding fields may be `null`.

In the case of fields appearing with type `relationship`, this property should apply recursively: such fields should appear in the response body as a nested array/object of record(s) (depending on whether the relationship is an object or array relationship), each with the appropriate fields described by the relationship field's `query` property.

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
| `binary_op`     | `operator`, `column`, `value`  | Test the specified `column` against a single `value` using a particular binary comparison `operator` |
| `binary_arr_op` | `operator`, `column`, `values` | Test the specified `column` against an array of `values` using a particular binary comparison `operator` |
| `unary_op`      | `operator`, `column`           | Test the specified `column` against a particular unary comparison `operator` |

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

Columns (as used in `column` fields in `binary_op`, `binary_arr_op`, `unary_op` and in `column`-typed Values) are specified as a column `name`, as well as a `path` to the table that contains the column. This path is an array of relationship names that starts from the table being queried (ie the table being queried by the query that this where expression is being specified in). An empty array means the column would be on the table being queried itself.

Here is a simple example, which correponds to the predicate "`first_name` is John and `last_name` is Smith":

```json
{
  "type": "and",
  "expressions": [
    {
      "type": "binary_op",
      "operator": "equal",
      "column": {
        "path": [],
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
        "path": [],
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
  "type": "and",
  "expressions": [
    {
      "type": "binary_op",
      "operator": "equal",
      "column": {
        "path": [],
        "name": "first_name"
      },
      "value": {
        "type": "column",
        "column": {
          "path": [],
          "name": "last_name"
        }
      }
    }
  ]
}
```

#### Ordering

The `order_by` field specifies an array of zero-or-more _orderings_, each of which consists of a field to order records by, and an order which is either `asc` (ascending) or `desc` (descending).

If there are multiple orderings specified then records should be ordered lexicographically, with earlier orderings taking precedence.

For example, to order records principally by `last_name`, delegating to `first_name` in the case where two last names are equal, we would use the following `order_by` structure:

```json
[
  {
    "field": "last_name",
    "order_type": "asc"
  },
  {
    "field": "first_name",
    "order_type": "asc"
  }
]
```

#### Relationships

If the call to `GET /capabilities` returns a `capabilities` record with a `relationships` field then the query structure may include fields corresponding to relationships.

_Note_ : if the `relationships` capability is not present then `graphql-engine` will not send queries to this agent involving relationships.

Relationship fields are indicated by a `type` field containing the string `relationship`. Such fields will also include the name of the relationship in a field called `relationship`. This name refers to a relationship that is specified on the top-level query request object in the `table_relationships` field.

This `table_relationships` is a list of tables, and for each table, a map of relationship name to relationship information. The information is an object that has a field `target_table` that specifies the name of the related table. It has a field called `relationship_type` that specified either an `object` (many to one) or an `array` (one to many) relationship. There  is also a `column_mapping` field that indicates the mapping from columns in the source table to columns in the related table.

It is intended that the backend should execute the `query` contained in the relationship field and return the resulting record set as the value of this field, with the additional record-level predicate that any mapped columns should be equal in the context of the current record of the current table.

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
  "table": "Artist",
  "table_relationships": [
    {
      "source_table": "Artist",
      "relationships": {
        "ArtistAlbums": {
          "target_table": "Album",
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
    "order_by": [],
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
          "from": "albums",
          "order_by": [],
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
    "from": "albums",
    "order_by": [],
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

Back on the relationship field inside the query, there is another `query` field. This indicates the query that should be executed against the `Album` table, but we must remember to enforce the additional constraint between Artist's `ArtistId` and Album's `ArtistId`. That is, in the context of any single outer `Artist` record, we should populate the `Albums` field with the array of Album records for which the `ArtistId` field is equal to the outer record's `ArtistId` field.

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
              "table": "Customer",
              "object_relationships": [
                {
                  "name": "SupportRep",
                  "using": {
                    "manual_configuration": {
                      "remote_table": "Employee",
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
              "table": "Employee"
            }
          ],
          "configuration": {}
        }
      ]
    }
  }
}
```

Given this GraphQL query (where the `X-Hasura-Role` header is set to `user`):

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
  "table": "Customer",
  "table_relationships": [
    {
      "source_table": "Customer",
      "relationships": {
        "SupportRep": {
          "target_table": "Employee",
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
          "type": "binary_op",
          "operator": "equal",
          "column": {
            "path": ["SupportRep"],
            "name": "Country"
          },
          "value": {
            "type": "column",
            "column": {
              "path": [],
              "name": "Country"
            }
          }
        }
      ]
    }
  }
}
```

The key point of interest here is in the `where` field where we are comparing between columns. The first column's `path` is `["SupportRep"]` indicating that the `Country` column specified there is on the other side of the `Customer` table's `SupportRep` relationship (ie. to the `Employee` table). The related `Employee`'s `Country` column is being compared with `equal` to `Customer`'s `Country` column (as indicated by the `[]` path). So, in order to evaluate this condition, we'd need to join the `Employee` table using the `column_mapping` specified in the `SupportRep` relationship and if any of the related rows (in this case, only one because it is an `object` relation) contain a `Country` that is equal to Employee row's `Country`, then the `binary_op` evaluates to True and we don't filter out the row.

#### Type Definitions

The `QueryRequest` TypeScript type in the [reference implementation](./reference/src/types/index.ts) describes the valid request body payloads which may be passed to the `POST /query` endpoint. The response body structure is captured by the `QueryResponse` type.

### Health endpoint
Agents must expose a `/health` endpoint which must return a 204 No Content HTTP response code if the agent is up and running. This does not mean that the agent is able to connect to any data source it performs queries against, only that the agent is running and can accept requests, even if some of those requests might fail because a dependant service is unavailable.

However, this endpoint can also be used to check whether the ability of the agent to talk to a particular data source is healthy. If the endpoint is sent the `X-Hasura-DataConnector-Config` and `X-Hasura-DataConnector-SourceName` headers, then the agent is expected to check that it can successfully talk to whatever data source is being specified by those headers. If it can do so, then it must return a 204 No Content response code.
