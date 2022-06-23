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
              "table": "albums",
              "object_relationships": [
                {
                  "name": "artist",
                  "using": {
                    "manual_configuration": {
                      "remote_table": "artists",
                      "column_mapping": {
                        "artist_id": "id"
                      }
                    }
                  }
                }
              ]
            },
            {
              "table": "artists",
              "array_relationships": [
                {
                  "name": "albums",
                  "using": {
                    "manual_configuration": {
                      "remote_table": "albums",
                      "column_mapping": {
                        "id": "artist_id"
                      }
                    }
                  }
                }
              ]
            }
          ],
          "configuration": {
            "tables": [ "artists", "albums" ]
          }
        }
      ]
    }
  }
}
```

The `backend_configs.dataconnector` section lets you set the URIs for as many agents as you'd like. In this case, we've defined one called "reference". When you create a source, the `kind` of the source should be set to the name you gave the agent in the `backend_configs.dataconnector` section (in this case, "reference").

The `configuration` property under the source can contain an 'arbitrary' JSON object, and this JSON will be sent to the agent on every request via the `X-Hasura-DataConnector-Config` header. The example here is configuration that the reference agent uses. The JSON object must conform to the schema specified by the agent from its `/config-schema` endpoint.

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

The `/schema` and `/query` endpoints require the request to have the `X-Hasura-DataConnector-Config` header set. That header contains configuration information that agent can use to configure itself. For example, the header could contain a connection string to the database, if the agent requires a connection string to know how to connect to a specific database. The header must be a JSON object, but the specific properties that are required are up to the agent to define.

The `/schema` and `/query` endpoints also require the request to have the `X-Hasura-DataConnector-SourceName` header set. This header contains the name of the data source configured in HGE that will be querying the agent. This can be used by the agent to maintain things like connection pools and configuration maps on a per-source basis.

We'll look at the implementation of each of the endpoints in turn.

### Capabilities and configuration schema

The `GET /capabilities` endpoint is used by `graphql-engine` to discover the capabilities supported by the agent, and so that it can know the correct shape of configuration data that needs to be collected from the user and sent to the agent in the `X-Hasura-DataConnector-Config` header. It should return a JSON object similar to the following:

```json
{
  "capabilities": {
    "relationships": true
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
      "name": "artists",
      "primary_key": "id",
      "columns": [
        {
          "name": "id",
          "type": "string",
          "nullable": false
        },
        {
          "name": "name",
          "type": "string",
          "nullable": false
        }
      ]
    },
    {
      "name": "albums",
      "primary_key": "id",
      "columns": [
        {
          "name": "id",
          "type": "string",
          "nullable": false
        },
        {
          "name": "title",
          "type": "string",
          "nullable": false
        },
        {
          "name": "artist_id",
          "type": "string",
          "nullable": false
        }
      ]
    }
  ]
}
```

The `tables` section describes the two available tables, as well as their columns, including types and nullability information.

Notice that the names of tables and columns are used in the metadata document to describe tracked tables and relationships.

#### Type definitions

The `SchemaResponse` TypeScript type from [the reference implementation](./reference/src/types/schema.ts) describes the valid response body for the `GET /schema` endpoint.

### Responding to queries

The `POST /query` endpoint is invoked when the user requests data from `graphql-engine` which is resolved by the service.

The service logs queries from the request body in the console. Here is a simple example based on a GraphQL query which fetches all artist data:

```graphql
query {
  artists {
    id name
  }
}
```

and here is the resulting query payload:

```json
{
  "from": "artists",
  "where": {
    "expressions": [],
    "type": "and"
  },
  "order_by": [],
  "limit": null,
  "offset": null,
  "fields": {
    "id": {
      "type": "column",
      "column": "id"
    },
    "name": {
      "type": "column",
      "column": "name"
    }
  }
}
```

The implementation of the service is responsible for intepreting this data structure and producing a JSON response body which is compatible with both the query and the schema.

Let's break down the request:

- The `from` field tells us which table to fetch the data from, namely the `artists` table.
- The `where` field tells us that there is currently no (interesting) predicate being applied to the rows of the data set (just an empty conjunction, which ought to return every row).
- The `order_by` field tells us that there is no particular ordering to use, and that we can return data in its natural order.
- The `limit` and `offset` fields tell us that there is no pagination required.
- The `fields` field tells us that we ought to return two fields per row (`id` and `name`), and that these fields should be fetched from the columns with the same names.

#### Response Body Structure

The response body for a call to `POST /query` should always consist of a JSON array of JSON objects. Each of those JSON objects should contain all of the same keys as the `fields` property of the request body, even if the values of the corresponding fields may be `null`.

In the case of fields appearing with type `relationship`, this property should apply recursively: such fields should appear in the response body as a nested array of records, each with the appropriate fields described by the relationship field's `query` property.

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

Here is a simple example, which correponds to the predicate "`first_name` is John and `last_name` is Smith":

```json
{
  "type": "and",
  "expressions": [
    {
      "type": "binary_op",
      "operator": "equal",
      "column": "first_name",
      "value": {
        "type": "scalar",
        "value": "John"
      }
    },
    {
      "type": "binary_op",
      "operator": "equal",
      "column": "last_name",
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
      "column": "first_name",
      "value": {
        "type": "column",
        "column": "last_name"
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

If the call to `GET /schema` returns a `capabilities` record with the `relationships` field set to `true`, then the query structure may include fields corresponding to relationships.

_Note_ :if the `relationships` field is set to `false` then `graphql-engine` will attempt to split queries into separate queries whenever relationships are involved. This is not always possible, depending on the structure of the query, but the fastest path to adding a data source as a Data Connector is to set `relationships` to `false` and to ignore this section. (This is currently not supported, but is intended to be in the future.)

Relationship fields are indicated by a `type` field containing the string `relationship`. Such fields will also include column_mapping` and `query` data.

`column_mapping` indicates the mapping from columns in the source table to columns in the related table. It is intended that the backend should execute the contained `query` and return the resulting record set as the value of this field, with the additional record-level predicate that any mapped columns should be equal in the context of the current record of the current table.

An example will illustrate this. Consider the following GraphQL query:

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

This will generate the following JSON query if `relationships` is set to `true`:

```json
{
  "where": {
    "expressions": [],
    "type": "and"
  },
  "offset": null,
  "from": "artists",
  "order_by": [],
  "limit": null,
  "fields": {
    "albums": {
      "type": "relationship",
      "column_mapping": {
        "id": "artist_id"
      },
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
          "title": {
            "type": "column",
            "column": "title"
          }
        }
      }
    },
    "name": {
      "type": "column",
      "column": "name"
    }
  }
}
```

Note the `albums` field in particular, which traverses the `artists` -> `albums` relationship:

```json
{
  "type": "relationship",
  "column_mapping": {
    "id": "artist_id"
  },
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
      "title": {
        "type": "column",
        "column": "title"
      }
    }
  }
}
```

The `column_mapping` field indicates the column mapping for this relationship, namely that the album's `artist_id` must equal the artist's `id`.

The `query` field indicates the query that should be executed against the `albums` table, but we must remember to enforce the additional constraint between `artist_id` and `id`. That is, in the context of any single outer `artist` record, we should populate the `albums` field with the array of album records for which the `artist_id` field is equal to the outer record's `id` field.

#### Type Definitions

The `Query` TypeScript type in the [reference implementation](./reference/src/types/query.ts) describes the valid request body payloads which may be passed to the `POST /query` endpoint. The response body structure is captured by the `QueryResponse` type.
