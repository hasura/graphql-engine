---

authors:
- Tiru <tiru@hasura.io>
- Rakesh <rakesh@hasura.io>

---

**Preview**: If you are interested in trying out a preview of this feature, please fill this form: https://bit.ly/hasura-dynamic-db and we will reach out with a preview build.

# Dynamic Connection Routing

Currently, Hasura only executes queries against a connection that was configured when the source is added in the metadata.
This proposal extends the functionality of graphql-engine to allow queries to be executed against a connection that is dynamically resolved at runtime.

## Index
- [Motivation](#motivation)
  * [Connect with different database credentials](#connect-with-different-database-credentials)
  * [No-stale reads when read-replicas are configured](#no-stale-reads-when-read-replicas-are-configured)
  * [Route to a specific shard or node-group in a distributed database](#route-to-a-specific-shard-or-node-group-in-a-distributed-database)
- [Solution](#solution)
  * [Important Note](#important-note)
- [Kriti Template](#kriti-template)
  * [Template context](#template-context)
    + [Request variables](#request-variables)
  * [Template outcome](#template-outcome)
    + [1. Connection Set](#1-connection-set)
    + [2. Primary connection](#2-primary-connection)
    + [3. Read replicas](#3-read-replicas)
    + [4. Default behavior](#4-default-behavior)
  * [Use cases](#use-cases)
    + [1. Connect with different database credentials](#1-connect-with-different-database-credentials)
    + [2. No-stale reads when read-replicas are configured](#2-no-stale-reads-when-read-replicas-are-configured)
    + [3. Route to different instances in a distributed system](#3-route-to-different-instances-in-a-distributed-system)
- [Runtime behavior](#runtime-behavior)
- [Metadata API](#metadata-api)
  * [Versioning](#versioning)
  * [Sample Postgres source metadata](#sample-postgres-source-metadata)
- [Test template API](#test-template-api)
  * [Example](#example)
- [Limitations](#limitations)
  * [1. Postgres schema of connection set](#1-postgres-schema-of-connection-set)
  * [2. Event triggers](#2-event-triggers)
  * [3. Migrations](#3-migrations)
- [Future enhancements](#future-enhancements)
  * [MSSQL sources](#mssql-sources)
  * [Raw URL string](#raw-url-string)
    + [Caching connections of URL strings](#caching-connections-of-url-strings)
  * [Custom context](#custom-context)
  * [Console](#console)
  * [Enhancements to connection set](#enhancements-to-connection-set)
    + [Health checks](#health-checks)
    + [Event triggers](#event-triggers)
    + [Migrations](#migrations)

## Motivation

Let's consider the following use-cases:

### Connect with different database credentials

If you are integrating with a vendor IAM or wish to use database RLS then you will want to use different connection credentials
per request. For example, in Hasura you may use a session-variable like x-hasura-role to use a specific connection string for that role.

### No-stale reads when read-replicas are configured

As of date, if read-replicas are configured then all query operations are routed to the read-replicas which sometimes leads
to stale reads because of replication lag. You can force certain query operations to the primary connection by using some
operation context variable like operation name or a special request header.

### Route to a specific shard or node-group in a distributed database

Route the query to specific node in a distributed database system like yugabyte/cockroach.

## Solution

The idea is to supply a [Kriti template](#kriti-template) within the source configuration which will be resolved during runtime for
any **non-admin** graphql query and the query will be executed against the resolved connection. A "resolved connection" can be any one of
- A reference to existing [primary connection](#2-primary-connection) (`connection_info`) or [read-replicas](#3-read-replicas)
- A [connection set](#1-connection-set) member
- A direction to fallback to [current behavior](#4-default-behavior)

The "connection set" is a list of connections predefined in the metadata so that
- a) it's easy to refer to it in the template and
- b) additional properties like conneciton pool settings can also be supplied.

### Important Note

---

The "primary" connection url (`connection_info`) is still required with a source configuration to run system queries like catalog introspection, metadata queries, event triggers, etc.

---
## Kriti Template

[Kriti-lang](https://github.com/hasura/kriti-lang) is a templating language developed by Hasura, inspired by Go templates.
It is used to transform the payloads of [actions](https://hasura.io/docs/latest/actions/rest-connectors/) and
[event triggers](https://hasura.io/docs/latest/event-triggers/rest-connectors/), aka Rest Connectors.

### Template context

Hasura provides a well-defined template context. Users should be able to design connection templates based on the template context and
according to their use cases. Template context contains the following variables.

#### Request variables
Predominantly in most of the use cases, the connection is being defined based on the various request parameters. Users need to have
necessary request variables to design the templates. Request context is a JSON object provided under top-level `request` field.
```json
{
    "request": "<RequestContextObject>"
}
```
The request object contains

1. **HTTP headers**:

HTTP client headers context is available as a JSON object with header names as fields and header values as JSON string values.
Users can refer to the headers in the template using `$.request.headers` JSON path variable.
Header names are case insensitive and hence their names are provided in lower case in the template context.

Context:

```json
{
    "request": {
        "headers": {
            "header_name_1": "header_value_1",
            "header_name_2": "header_value_2"
        }
    }
}
```
Example:

```
{{ if ($.request.headers.my_header == "somevalue")}}
  <do something here>
{{ else }}
  <do something here>
```

2. **Hasura session variables**:

[Session variables](https://hasura.io/docs/latest/auth/authorization/roles-variables/#dynamic-session-variables) are key-value pairs
returned from the authentication service for each request. Keys always contain `x-hasura-*` as prefix. Values are of string type.
The `x-hasura-role` session variable determines the [role](https://hasura.io/docs/latest/auth/authorization/roles-variables/#roles)
of the GraphQL request.

Context:
```json
{
    "request": {
        "session": {
            "x-hasura-user-id": "1",
            "x-hasura-role": "manager"
        }
    }
}
```
Example:

```
{{ if ($.request.session.x-hasura-role == "manager")}}
  <do something here>
{{ elif ($.request.session.x-hasura-user-id == "1")}}
  <do something here>
{{ else }}
  <do something here>
```

3. **GraphQL query parameters**:

Including certain GraphQL query parameters like [operation type](https://spec.graphql.org/October2021/#OperationType) and
[operation name](https://spec.graphql.org/October2021/#sec-Named-Operation-Definitions) (optional), in the template context, is useful to write
conditionals in kriti template.

Context:
```json
{
    "request": {
        "query": {
            "operation_type": "query",
            "operation_name": "getUsersById"
        }
    }
}
```
The `operation_type` will be any one of the following values:
- `query`
- `subscription`
- `mutation`

Example:
```
{{ if ($.request.query.operation_type == "query") || ($.request.query.operation_type == "subscription") }}
    <do something here>
{{ else }}
    <do something here>
{{ end }}
```

### Template outcome
Hasura expects any one of the following outcome from a template resolution.

#### 1. Connection Set
Users can pre-define a set of connections in the source metadata and refer to them in the template. Hasura includes the connection set
info in the template context under top-level `connection_set` field.

Template variable: `$.connection_set.name`

Example:
```
{{ if ($.request.session.x-hasura-role == "somerole") }}
    {{$.connection_set.some_name}}
{{ else }}
    {{$.primary}}
{{ end }}
```

Metadata configuration: A JSON object with `key` being the connection name and `value` is
[connection info object](https://hasura.io/docs/latest/api-reference/syntax-defs/#pgsourceconnectioninfo).


```json
{
  "configuration": {
    "connection_info": "<ConnectionInfoObject>",
    "connection_set": [
      {
        "name": "connection_name_1",
        "connection_info": "<ConnectionInfoObject>"
      },
      {
        "name": "connection_name_2",
        "connection_info": "<ConnectionInfoObject>"
      }
    ]
  }
}
```
See [Metadata API](#metadata-api) for more information.

Hasura enables **connection pooling** for all members of connection set. Users can configure the `pool_settings` in
the [`ConnectionInfoObject`](https://hasura.io/docs/latest/api-reference/syntax-defs/#pgsourceconnectioninfo).

#### 2. Primary connection
Connection data provided under `connection_info` configuration field in source metadata is referred as **primary connection**.
Users can design template to direct Hasura to use the primary connection for query execution.

Template variable: `$.primary`

Example:
```
{{ if ($.request.session.x-hasura-role == "somerole") }}
    {{$.connection_set.some_connection}}
{{ else }}
    {{$.primary}}
{{ end }}
```

Advantages of `$.primary`:
- Users don't need to re-define the `connection_info` specs in the connection set.
- Hasura re-uses the primary connection **pool** which aids query performance.

#### 3. Read replicas
Like primary connection, Hasura also provides the context for read-replicas.

Template variable: `$.read_replicas`

When a template resolves to `$.read_replicas` variable, the query will be executed on randomly choosen read-replica connection.

Example:
```
{{ if ($.request.session.x-hasura-role == "somerole") || ($.request.query.operation_type == "query")}}
    {{$.read_replicas}}
{{ else }}
    {{$.primary}}
{{ end }}
```

The `$.read_replicas` also have similar advantages to those of the `$.primary`.

#### 4. Default behavior
In template, users may want to direct Hasura to fallback to default/current connection behavior, i.e,
- If defined, use `read_replicas` for all queries and subscriptions.
- Rest all queries use `primary` connection

Using the `$.default` path variable, users can refer to default connection behaviour in the template. It is highly beneficial to
users when they want to dynamically route only a small set of queries and rest all other queries will use default connection behavior.

Example:
```
{{ if ($.request.session.x-hasura-role == "user")}}
  {{$.primary}}
{{ else }}
  {{$.default}}
```


**Variable usage:**
Users are expected to use template outcome variables only in expressions without any string interpolation or control flow syntax.

The following templates are **invalid**:

1. String interpolation:
```
"{{$.connection_set.some_name}}?param=value"
```
2. In control-flow syntax:
```
{{ if ($.read_replicas == "some_value") }}
    {{$.primary}}
{{ else }}
    {{$.default}}
{{ end }}
```


### Use cases

#### 1. Connect with different database credentials

Define the connections with different database credentials in [connection_set](#1-connection-set) and refer to them in the template.
```
{{ if ($.request.session.x-hasura-role == "manager")}}
    {{$.connection_set.manager_db}}
{{ elif ($.request.session.x-hasura-role == "employee")}}
    {{$.connection_set.employee_db}}
{{ else }}
    {{$.default}}
{{ end }}
```

#### 2. No-stale reads when read-replicas are configured

For queries and subscriptions, route the query to `primary` connection based on a custom request variable.
```
{{ if (($.request.query.operation_type == "query") || ($.request.query.operation_type == "subscription")) && ($.request.headers.x-query-read-no-stale == "true") }}
    {{$.primary}}
{{ else }}
    {{$.default}}
{{ end }}
```
If `x-query-read-no-stale` header is set to `true` value, all queries and subscriptions use primary connection.
Otherwise the default behavior is choosen.

#### 3. Route to different instances in a distributed system

Define the database instances in [connection_set](#1-connection-set) and refer to them in the template.

```
{{ if ($.request.headers.x-db-tenant-id == "name_1")}}
    {{$.connection_set.database_name_1}}
{{ elif ($.request.session.x-hasura-role == "name_2")}}
    {{$.connection_set.database_name_2}}
{{ else }}
    {{$.default}}
{{ end }}
```

## Runtime behavior

If user defines the kriti template in the source metadata, all **non-admin** GraphQL query requests served
via `/v1/graphql` and `/v1beta1/relay` endpoints, including subscriptions, are executed on resolved connections.
Hasura raises runtime execution error when the template
- Fails to resolve due to
  - Incorrect kriti syntax
  - Invalid JSON path variables
- Resolves to
  - an invalid JSON
  - an unexpected outcome

## Metadata API

Users can add a new connection template or update existing connection template via `pg_add_source` or `pg_update_source` metadata API.
Introducing a new field, `connection_template` inside `configuration` object of source metadata, where the template can be defined.

```http
POST /v1/metadata HTTP/1.1
Content-Type: application/json
X-Hasura-Role: admin

{
  "type": "pg_add_source",
  "args": {
    "name": "pg1",
    "configuration": {
      "connection_info": {
        "database_url": {
           "from_env": "<DB_URL_ENV_VAR>"
         }
      },
      "connection_template": {
          "template": "<Kriti Template>"
      }
    }
  }
}
```
Adding connection template with [connection_set](#1-connection-set).

```http
POST /v1/metadata HTTP/1.1
Content-Type: application/json
X-Hasura-Role: admin

{
  "type": "pg_add_source",
  "args": {
    "name": "pg1",
    "configuration": {
      "connection_info": {
        "database_url": {
           "from_env": "<DB_URL_ENV_VAR>"
         }
      },
      "connection_template": {
        "template": "<Kriti Template>"
      },
      "connection_set": [
        {
          "name": "name_1",
          "connection_info": {
            "database_url": {
              "from_env": "<DB_URL_ENV_VAR_NAME_1>"
            },
            "pool_settings": {
              "max_connections": 50,
              "idle_timeout": 180,
              "retries": 1,
              "pool_timeout": 360,
              "connection_lifetime": 600
            }
          }
        },
        {
          "name": "name_2",
          "connection_info": {
            "database_url": {
              "from_env": "<DB_URL_ENV_VAR_NAME_2>"
            }
          }
        }
      ]
    }
  }
}
```

### Versioning

A kriti template is valid as long as the JSON path variables are in coherent with the template context. In case of any
future enhancement to template context, Hasura will support existing templates by providing compatible template context
based on version. The version will be a positive integer and bumped up by `1` when there's breaking change in template context.

```json
"connection_template": {
  "version": 1
  "template": "<Kriti Template>"
}
```

### Sample Postgres source metadata

Find a specimen Postgres source metadata with connection template and connection set below
```yaml
name: pg_source_name
kind: postgres
tables: []
configuration:
  connection_info:
    use_prepared_statements: false
    database_url:
      from_env: PG_DATABASE_URL
    solation_level: read-committed
  read_replicas:
  - database_url:
      from_env: "<DATABASE_REPLICA_URL_ENV_1>"
  - database_url:
      from_env: "<DATABASE_REPLICA_URL_ENV_2>"
  connection_set: # new metadata field
  - name: my_tenant_1
    connection_info:
      database_url:
        from_env: PG_DATABASE_URL_TENANT_1
  - name: my_tenant_2
    connection_info:
      database_url:
        from_env: PG_DATABASE_URL_TENANT_2
  connection_template: # new metadata field
    version: 1
    template: |
      {{ if ($.request.session.x-hasura-tenant-id == "my_tenant_1")}}
        {{$.connection_set.my_tenant_1}}
      {{ elif ($.request.session.x-hasura-tenant-id == "my_tenant_2")}}
        {{$.connection_set.my_tenant_2}}
      {{ else }}
        {{$.default}}
      {{ end }}
```

## Test template API

An admin-only metadata API to test connection templates.
The API works similar to the [test_webhook_transform](https://hasura.io/docs/latest/api-reference/metadata-api/manage-metadata/#test-webhook-transform) API.
Payload contains necessary information to resolve the template such as request headers, session variables and GraphQL query parameters like query type and operation name.

### Example

Request:
```http
POST /v1/metadata HTTP/1.1
Content-Type: application/json
X-Hasura-Role: admin

{
  "type": "pg_test_connection_template",
  "args": {
    "source": "source_name",
    "request_context": {
      "headers": {
        "header_name": "header_value"
      },
      "session": {
        "session_var": "session_var_value"
      },
      "query": {
        "operation_type": "query",
        "operation_name": "op_name"
      }
    }
  }
}
```

Success Response:
```json
{
  "result": {
    "routing_to": "<primary | read_replicas | connection_set | default>",
    "value": "< null | connection_set_member_name>"
  }
}
```

Error Response:

```json
{
  "path": "$"
  "code": "template-resolution-failed",
  "error": "Session variable x-hasura-something is expected, but not found."
}
```

## Limitations

### 1. Postgres schema of connection set

Hasura derives the GraphQL schema based on only primary connection, the `connection_info`. The Postgres schema of all members of
`connection_set` should be identical to that of the primary. Hasura does not make any checks to ensure the Postgres schema consistency
and users should guarantee the same. A GraphQL request may result in runtime exception when it is being executed on a member of
connection set that differs in Postgres schema from the primary.

### 2. Event triggers

Hasura events are triggered only for mutations executed on the primary. A [possible solution](#event-triggers) is mentioned in the
future enhancements section.

### 3. Migrations

CLI migrations cannot be applied on a connection set member. A [possible solution](#migrations) is mentioned in the future enhancements section.

## Future enhancements

### MSSQL sources

Extend connection templates and connection set to MSSQL sources. The template context will remain same as MSSQL sources also
have support for read replicas.

### Raw URL string

Allow connections template returning a raw URL string that can be used to establish connection and execute the query. A template can
possibly resolve to many number of URL strings. It is very complex and resource intensive to pool each and every URL string.

```
"postgres://{{$.request.session.x-hasura-role}}:{{$.env.POSTGRES_DB_PASSWORD}}@db.com:5432/dbname"
```

#### Caching connections of URL strings

If the template resolves to a raw connection URL, Hasura will establish a connection,
executes the query and closes the connection. Performance of the queries are compromised here. In future iterations,
cache the connection for certain time period and use it for subsequent requests. This sounds similar to connection
pooling, but here the caching differs in storing connections of different URL strings. Though, for now, the idea is pretty vague
and can be improved in the future.


### Custom context

Allow users to define a custom JSON context in the metadata.
```json
"connection_template": {
  "version": 1
  "template": "<Kriti Template>",
  "custom_context": {
    "database_host_east": "east_db.com",
    "database_host_west": "west_db.com"
  }
},
```
Custom context can be referred in the template using `$.custom_context` variable prefix.
```
{{ if ({{$.request.headers.x-client-origin}}  == "east_app") }}
    "postgres://username:password@{{$.custom_context.database_host_east}}/dbname"
{{ elif ({{$.request.headers.x-client-origin}}  == "west_app") }}
    "postgres://username:password@{{$.custom_context.database_host_west}}/dbname"
{{ else }}
    {{$.default}}
{{ end }}
```

### Console

Enhance the Hasura console UI experience by
- Syntax highlighting the kriti-lang.
- Auto-completing kriti syntax, like `{{ if }} {{ else }} ... {{ end }}` and template variables.

### Enhancements to connection set

#### Health checks

Extend [source health checks](https://hasura.io/docs/latest/deployment/health-checks/source-health-check/) to members of [connection set](#1-connection-set).
Include connection set in health check reports. Users will realize when Hasura is unable to establish connection to a member of connection set.


#### Event triggers

Initially, mutations on a member of connection set do not trigger a Hasura event. Introduce an option to enable event triggers on a
connection set member. Hasura will maintain a state (source catalog) in the database to enable eventing.

#### Migrations

Extend the `<backend>_run_sql` query API to execute the SQLs on members of connection set, thus allowing CLI migrations.
