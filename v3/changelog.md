# Changelog

## [Unreleased]

### Changed

### Fixed

### Added

## [v2025.08.18]

### Fixed

- Fixes to `_and` filters inside nested arrays.

This filter means "return an item when there is a user inside 'users' with first
name 'Bruce' and last name 'Willis'".

```graphql
where: { users: { _and: [
  { first_name: { _eq: "Bruce" },
  { last_name: { _eq: "Willis" }
]}}
```

This filter means "return an item when there is a user inside 'users' with first
name 'Bruce' and there is a user inside 'users' with last name 'Willis'".

```graphql
where: { _and: [
  { users: { first_name: { _eq: "Bruce" } },
  { users: { last_name: { _eq: "Willis" } }
]}}
```

Previously both examples would be treated as the latter, which was incorrect,
but now each is treated correctly once the OpenDD flag
`fix_exists_in_nested_arrays` is enabled.

## [v2025.08.14-1]

- No changes

## [v2025.08.14]

### Fixed

- Fixed an issue in remote joins where a command that returns headers on the
  left hand side of the join wouldn't correctly pass arguments to the right hand
  side.

- Fix a bug where remote relationships from fields with aliases wouldn't work.

### Added

- Added `NDC_RESPONSE_SIZE_LIMIT` that allows limiting response sizes, set in
  bytes. Defaults to 30MB.

## [v2025.08.13]

### Added

- Add support for wildcard at leaf subdomain level in allowed CORS origins. Eg.
  `https://*.example.com` will allow `https://api.example.com`,
  `https://auth.example.com`, and so on.

## [v2025.07.29]

No changes since last release.

## [v2025.07.28]

### Synchronous Mode Pre-Response Plugins

Added support for synchronous pre-response plugins that can intercept and modify
GraphQL responses before they are sent to clients.

**Key Features:**

- **Response Modification**: Plugins can transform response content and pass
  modified responses to subsequent plugins in the chain
- **Sequential Execution**: Plugins execute in order, allowing response
  transformations to build upon each other
- **Flexible Response Handling**: Support for multiple HTTP status codes:
  - `204 No Content`: Continue with original response
  - `200 OK`: Continue with modified response (JSON body)
  - `400 Bad Request`: Return user error to client
  - `500 Internal Server Error`: Return internal error
- **On-Plugin-Failure Support**: Configure plugins to fail gracefully without
  breaking the entire request

**Configuration:** Following is an example of the OpenDD metadata for the
pre-response plugin in synchronous mode:

```yaml
kind: LifecyclePluginHook
version: v1
definition:
  pre: response
  name: test-1
  url:
    value: http://localhost:5001/extensions/add_ai_summary
  config:
    request:
      session: {}
      rawRequest:
        query: {}
        variables: {}
    mode:
      type: synchronous
      onPluginFailure: continue
```

The `mode` field in the `LifecyclePreResponsePluginHook` configuration allows
specifying the mode of the plugin hook.

- `type` (`string`): The type of the mode. Can be either `synchronous` or
  `asynchronous` (default).
- `onPluginFailure` (`string`): The behavior of the engine when the plugin
  returns 400 or 500. Can be one of `fail` (default) or `continue`. This field
  is only for `synchronous` pre-response plugins.

## [v2025.07.22]

### Changed

- Check data connector capabilities for nested scalar comparisons in `plan`
  stage so any errors are returned as user rather than internal errors.

## [v2025.07.21]

### Added

- Allow pre-parse plugins to modify the request before it is sent to the engine.
  Use HTTP status code `299` to indicate that the request should be modified.

## [v2025.07.14]

### Fixed

- Fixed arguments passed to command relationships

```graphql
query MyQuery {
  Analytics {
    ActorByMovieIdBounds(upper_bound: 7) {
      name
    }
  }
}
```

In this query, `ActorByMovieIdBounds` is a relationship from the `Analytics`
model to a `Command` that takes two arguments.

One argument, `lower_bound`, is provided by the relationship, but the
`upper_bound` is provided by the user in the query. Previously we were not
including the user's arguments in the generated plan, but now we are.

## [v2025.07.10]

### Changed

- Added `disallow_literals_as_boolean_expression_arguments` feature flag to
  disallow literals as arguments to boolean expression operators that expect a
  boolean expression.

### Fixed

- Fixed JWT authentication errors to return correct HTTP status codes. Expired
  tokens now return `400 Bad Request` instead of `500 Internal Server Error`.

## [v2025.07.07]

### Pre-NDC Request and Pre-NDC Response Plugins

Add support for `pre-ndc-request` and `pre-ndc-response` plugins. These plugins
allow HTTP webhooks to modify requests before they're sent to data connectors
and modify responses before they're processed by the engine.

##### Pre-NDC Request Plugin Behavior

The `pre-ndc-request` plugin is called before a request is sent to a data
connector.

Example metadata:

```yaml
kind: LifecyclePluginHook
version: v1
definition:
  pre: ndcRequest
  name: request_modifier # Plugin name must be unique
  connectors:
    - postgres # List of data connectors this plugin applies to. This is scoped to the current subgraph. Connectors can only be referenced by a single plugin of each type.
  url:
    value: http://localhost:5001/modify-request # Webhook URL to call
  config:
    request:
      headers:
        # Map of custom headers to send to the webhook
        hasura-m-auth:
          value: "your-strong-m-auth-key"
      session: {} # Include session information in the webhook request
      ndcRequest: {} # Include the NDC request in the webhook request
```

The plugin receives a request body with the following structure:

```json
{
  "session": {
    /* Session information if configured */
    "role": "user",
    "variables": {
      "x-hasura-user-id": "1"
    }
  },
  "ndcRequest": {
    /* The NDC request if configured */
  },
  "dataConnectorName": "qualified.connector.name",
  "operationType": "query|queryExplain|mutation|mutationExplain",
  "ndcVersion": "v0.1.x|v0.2.x"
}
```

The plugin can respond in the following ways:

1. HTTP 204 (No Content): The original request will be used without
   modification.
2. HTTP 200 (OK) with a JSON body containing either:
   - `{"ndcRequest": {...}}`: A modified request that will replace the original.
   - `{"ndcResponse": {...}}`: A response that will be used instead of calling
     the data connector.
3. HTTP 400 (Bad Request): The plugin encountered a user error, which will be
   returned to the client.
4. Any other status code: Treated as an internal error.

##### Pre-NDC Response Plugin Behavior

The `pre-ndc-response` plugin is called after receiving a response from a data
connector but before processing it.

Example metadata:

```yaml
kind: LifecyclePluginHook
version: v1
definition:
  pre: ndcResponse
  name: response_modifier # Plugin name must be unique
  connectors:
    - postgres # List of data connectors this plugin applies to. This is scoped to the current subgraph. Connectors can only be referenced by a single plugin of each type.
  url:
    value: http://localhost:5001/modify-response # Webhook URL to call
  config:
    request:
      headers:
        # Map of custom headers to send to the webhook
        hasura-m-auth:
          value: "your-strong-m-auth-key"
      session: {} # Include session information in the webhook request
      ndcRequest: {} # Include the original NDC request in the webhook request
      ndcResponse: {} # Include the NDC response in the webhook request
```

The plugin receives a request body with the following structure:

```json
{
  "session": {
    /* Session information if configured */
    "role": "user",
    "variables": {
      "x-hasura-user-id": "1"
    }
  },
  "ndcRequest": {
    /* The original NDC request if configured */
  },
  "ndcResponse": {
    /* The NDC response if configured */
  },
  "dataConnectorName": "qualified.connector.name",
  "operationType": "query|queryExplain|mutation|mutationExplain",
  "ndcVersion": "v0.1.x|v0.2.x"
}
```

The plugin can respond in the following ways:

1. HTTP 204 (No Content): The original response will be used without
   modification.
2. HTTP 200 (OK) with a JSON body containing the modified response.
3. HTTP 400 (Bad Request): The plugin encountered a user error, which will be
   returned to the client.
4. Any other status code: Treated as an internal error.

#### A note on request headers

Request headers are not forwarded to the `pre-ndc-response` and
`pre-ndc-request` plugins. If you need values from these headers in the plugins,
you should add the value in question to the session via an auth webhook.

## [v2025.07.02]

No changes since last release.

## [v2025.06.27]

No changes since last release.

## [v2025.06.26]

No changes since last release.

## [v2025.06.16]

### Added

#### Support for multiple authentication modes (AuthConfig v4)

AuthConfig v4 is a new version of the AuthConfig that allows for multiple
authentication modes. The default mode is specified in the `mode` field, and
alternative modes are specified in the `alternativeModes` field.

The following is an example of the OpenDD metadata for the AuthConfig v4:

```yaml
version: v4
definition:
  mode:
    noAuth:
      role: admin
      sessionVariables:
        x-hasura-user-id: "1"
  alternativeModes:
    - identifier: webhook
      config:
        webhook:
          url:
            value: http://auth_hook:3050/validate-request
          method: POST
```

The `X-Hasura-Auth-Mode` header can be used to specify the authentication mode
when making requests.

In the above example, if no `X-Hasura-Auth-Mode` header is specified, the
default mode (`noAuth`) will be used. If the header is specified, the
authentication mode specified in the header will be used if it exists in the
`alternativeModes` field. If the header is specified but the authentication mode
does not exist in the `alternativeModes` field, the default mode will be used.

**Note**: The AuthConfig v4 is backwards compatible with the AuthConfig v3.

## [v2025.06.04]

### Fixed

- Fixed a bug when missing variables for enum type arguments to functions and
  procedures did not obey the `validate_non_nullable_graphql_variables` feature
  flags.

## [v2025.05.29]

### Added

- When the `send_missing_arguments_to_ndc_as_nulls` flag is enabled, the engine
  will now send null values for missing arguments to the data connector rather
  than omitting them. Note that may be a breaking change for some data
  connectors, particularly `ndc-postgres` before `v2.1.0`.

## [v2025.05.14]

### Fixed

- Properly support missing or null `predicate` in relationship filters, to align
  with
  [the docs](https://hasura.io/docs/3.0/reference/metadata-reference/permissions/#modelpermissions-relationshippredicate).

## [v2025.05.13]

### Fixed

- Fixed a bug where scalar type lookups in remote relationships would fail due
  to looking up in the source rather than target data connector

## [v2025.04.30]

### Fixed

### Changed

### Added

## [v2025.04.28]

### Fixed

- Fixed regression where `is_null` calls on nested array fields would throw an
  error

## [v2025.04.23]

### Added

- Clearer error messages when defining a comparable relationship to a model that
  has no filter expression defined

## [v2025.04.14]

### Fixed

- Fixed a bug in which using a graphql alias in a query involving a remote
  relationship would result in a runtime error

## [v2025.04.02]

### Added

- Add data connector error details to trace error messages. This will help
  debugging data connector errors.
- Add a flag `validate_non_null_graphql_variables` to enable runtime validations
  for non-nullable GraphQL variables.
- Improved metadata JSON deserialization errors by adding more contextual
  information.

### Changed

- Model permissions can now reference nested object fields. For instance, to
  select all institutions where the country of their location is "UK":

```yaml
kind: ModelPermissions
version: v1
definition:
  modelName: institutions
  permissions:
    - role: admin
      select:
        filter:
          nestedField:
            fieldName: location
            predicate:
              nestedField:
                fieldName: country
                predicate:
                  fieldComparison:
                    field: name
                    operator: _eq
                    value:
                      literal: UK
```

## [v2025.03.25]

### Fixed

- Apply validations for operators in scalar boolean expressions:
  - Disallow non-list argument types for the `_in` operator.
  - Argument type must match the scalar type for `_eq`, `_lt`, `_lte`, `_gt` and
    `_gte` operators.
  - Operators such as `contains`, `icontains`, `starts_with`, `istarts_with`,
    `ends_with`, and `iends_with` now only applicable on string scalars, with
    arguments strictly of type string.
  - Check if mapped operators exist in the data connector.
  - Check the argument type compatibility with the mapped operator's NDC
    argument type.
- The `value` alias for `literal` fields in `ValueExpression` and
  `ValueExpressionOrPredicate` was removed in a recent change, and is now
  reinstated.

## [v2025.03.20]

### Changed

- Improved error messages for ModelPermissions build errors that include more
  contextual information

### Fixed

- JSON paths in metadata parse errors involving externally tagged unions (such
  as `BooleanExpressionOperand`) now correctly include the tag property in the
  path.
- ModelPermissions no longer allow roles to be defined more than once

## [v2025.03.17]

### Added

- Added `/v1/jsonapi` as an alias for `/v1/rest` endpoints to better reflect the
  JSONAPI specification compliance.

## [v2025.03.13]

### Changed

- Data connectors resolve step now returns multiple errors rather than failing
  on the first one.
- Improved build errors for when older versions of the ddn CLI generate invalid
  metadata in the DataConnectorLink; the new error guides users to upgrade their
  CLI version.
- Scalar types resolve step now returns multiple errors rather than failing on
  the first one.

### Fixed

- Remote joins that map fields that contain objects now correctly pass the whole
  object contained in that field to the target data connector, instead of
  sometimes a subset based on what's been selected in the GraphQL query. In
  addition, the fields of the object are now mapped correctly to the target data
  connector's field names.

## [v2025.03.11]

### Added

- Add error contexts and paths to some argument errors for Models and Commands.

### Changed

- Empty filter expressions are consistent between old and OpenDD execution
  pipelines

- Fixed error responses from JSON:API by making it spec-compliant.

## [v2025.03.10]

### Added

#### Native Data Connector (NDC) Specification v0.2.0 Support

Engine now supports the
[NDC specification v0.2.0](https://hasura.github.io/ndc-spec/specification/changelog.html#020).
This new version of the specification adds many new capabilities to data
connectors that engine will support over time. As of this release, engine
supports the following new features for data connectors that support NDC Spec
v0.2.0:

##### Filtering by items in a nested array of scalars

Consider an `ObjectType` with a field that is an array of scalars, such as the
`Country` type with its `cities` array of string field below.

```yaml
kind: ObjectType
version: v1
definition:
  name: Country
  fields:
    - name: id
      type: Int!
    - name: name
      type: String!
    - name: cities
      type: [String!]!
  graphql:
    typeName: Country
  dataConnectorTypeMapping: {}
```

It is now possible to write the following `where` clause in GraphQL, which
returns all countries whose cities array contains a string equalling
"Melbourne":

```graphql
query {
  CountryMany(where: { cities: { _eq: "Melbourne" } }) {
    id
    name
    cities
  }
}
```

In order to do this, the `cities` field must be added as a `comparableField` to
Country's `BooleanExpressionType`:

```yaml
kind: BooleanExpressionType
version: v1
definition:
  name: Country_bool_exp
  operand:
    object:
      type: Country
      comparableFields:
        - fieldName: cities
          booleanExpressionType: String_bool_exp # New!
  logicalOperators:
    enable: true
  isNull:
    enable: false
  graphql:
    typeName: Country_bool_exp
```

The `booleanExpressionType` of the `comparableField` must be for the scalar type
of the array element (in this case a `BooleanExpressionType` for `String`).

However, doing this is only supported if the data connector your `Model` sources
itself from supports filtering by nested scalar arrays, which is new in NDC Spec
v0.2.0.

##### Count aggregates can return ScalarTypes other than Int

Some data sources return count aggregates using a scalar type other than a
32-bit integer (for example, a 64-bit integer). If the data connector declares
that counts are returned in a different scalar type, the engine can now return
those counts using the approprate ScalarType.

This is done by specifying the correct ScalarType in the `AggregateExpression`
`count` and `countDistinct`'s `resultType` field, like so:

```yaml
kind: AggregateExpression
version: v1
definition:
  name: String_aggregate_exp
  operand:
    scalar:
      aggregatedType: String
      aggregationFunctions:
        - name: _min
          returnType: String
        - name: _max
          returnType: String
      dataConnectorAggregationFunctionMapping:
        - dataConnectorName: custom
          dataConnectorScalarType: String
          functionMapping:
            _min:
              name: min
            _max:
              name: max
  count:
    enable: true
    returnType: Int64 # New!
  countDistinct:
    enable: true
    returnType: Int64 # New!
  description: Aggregate expression for the String type
  graphql:
    selectTypeName: String_aggregate_exp
```

If `returnType` is omitted, it defaults to the `Int` type.

##### Filtering and ordering of relationships that start from inside a nested object

Consider an `Institution` type with a nested `campuses` field, where each
`Campus` has a relationship to a `Country` type:

```yaml
kind: ObjectType
version: v1
definition:
  name: Institution
  fields:
    - name: id
      type: Int!
    - name: name
      type: String!
    - name: campuses
      type: "[Campus!]!" # Array of Campus objects

kind: ObjectType
version: v1
definition:
  name: Campus
  fields:
    - name: id
      type: Int!
    - name: name
      type: String!
    - name: country_id # Foreign key to Country
      type: Int!

kind: Relationship
version: v1
definition:
  name: country
  sourceType: Campus
  target:
    model:
      name: Country
      relationshipType: Object
  mapping:
    - source:
        fieldPath:
          - fieldName: country_id
      target:
        modelField:
          - fieldName: id
```

If the data connector declares support for doing so, you can filter and order
institutions based on their campuses' countries:

```graphql
query {
  InstitutionMany(
    where: { campuses: { country: { name: { _eq: "USA" } } } }
    order_by: { campuses: { country: { name: Asc } } }
  ) {
    id
    name
    campuses {
      name
      country {
        name
      }
    }
  }
}
```

To do so, you will need to declare the `country` relationship as a
`comparableRelationship` in the `Campus`'s `BooleanExpressionType`, an as an
`orderableRelationship` in the `Campus`'s `OrderByExpression`:

```yaml
kind: BooleanExpressionType
version: v1
definition:
  name: Campus_bool_exp
  operand:
    object:
      type: Campus
      comparableFields:
        - fieldName: id
          booleanExpressionType: Int_bool_exp
        - fieldName: name
          booleanExpressionType: String_bool_exp
      comparableRelationships:
        - relationshipName: country # New!
          booleanExpressionType: Country_bool_exp
  logicalOperators:
    enable: true
  isNull:
    enable: false
  graphql:
    typeName: Campus_bool_exp

kind: OrderByExpression
version: v1
definition:
  name: Campus_order_by_exp
  operand:
    object:
      orderedType: Campus
      orderableFields:
        - fieldName: id
          orderByExpression: Int_order_by_exp
        - fieldName: name
          orderByExpression: String_order_by_exp
      orderableRelationships:
        - relationshipName: country # New!
          orderByExpression: Country_order_by_exp
  graphql:
    expressionTypeName: Campus_order_by_exp
```

#### Other

- Relationships that target models are now able to provide mappings that target
  model arguments.
- Pretty print errors where they have had contexts and paths provided

### Fixed

- Validate `ObjectType` field types against mapped NDC types, ensuring
  compatibility of nullability, arrays, and scalar representations.
- Validate argument types against mapped NDC types, ensuring compatibility of
  nullability, arrays, and scalar representations.
- Validate `AuthConfig` headers to ensure they are valid HTTP headers.
- GraphQL API: Fixed a bug where `null` inputs for nullable query parameters
  like `limit`, `offset`, `order_by` and `where` would cause an error.
- Disallow relationships targeting procedure based commands.

### Changed

## [v2025.02.26]

### Added

- Added validation for command output types to ensure they reference valid types
  in the schema.

### Fixed

- Reverted: GraphQL API: Using `null` for non-nullable custom scalar types now
  correctly raises validation error.
- Reverted: GraphQL API: Validate query variables to ensure non-nullable
  variables are not omitted or set to `null`.

### Changed

## [v2025.02.20]

### Fixed

- GraphQL API: Using `null` for non-nullable custom scalar types now correctly
  raises validation error.

### Changed

## [v2025.02.19]

### Added

#### AuthConfig v3

AuthConfig v3 is a new version of the AuthConfig that allows for more
flexibility in the configuration of the authentication webhook.

The following is an example of the OpenDD metadata for the AuthConfig v3:

```yaml
version: v3
definition:
  mode:
    webhook:
      method: GET
      url:
        valueFromEnv: AUTH_HOOK_URL
      customHeadersConfig:
        headers:
          forward:
            - Authorization
          additional:
            user-agent: hasura-ddn
```

For the above example, the following headers will be sent to the auth hook:

- `Authorization` header from the original request
- `user-agent` header with the value `hasura-ddn`

The AuthConfig v3 is backwards compatible with the AuthConfig v2.

#### Other

- GraphQL WebSocket connections now include client headers from the initial
  handshake request, in addition to those from the `connection_init` message.
  These headers are forwarded to auth webhooks, plugins and data connectors.
- Remote relationships are now available in the JSONAPI.

### Fixed

- Support execution of remote joins from mutations/procedures

- Loosen restriction on `ObjectType` fields to allow them to refer to
  `BooleanExpressionType` with the `object` operand.

- Restrict `ObjectType`s that contain infinite recursion through non-nullable
  field paths. Warnings are emitted while creating a build. Projects created or
  with a
  [compatibility date](https://hasura.io/docs/3.0/supergraph-modeling/compatibility-config/)
  after this release date are affected.
- Avoid infinite recursion when validating `BooleanExpressionType` for
  `ObjectType`s that contain self references through nested fields during build
  creation.
- Argument presets did not map OpenDD field names to the corresponding NDC field
  names before sending values across, this is now resolved.
- GraphQL API: Validate query variables to ensure non-nullable variables are not
  omitted or set to `null`.

### Changed

- When JWKs are configured to be read from a URL in AuthConfig, we now no longer
  require the JWT to specify the `alg` property. We will validate the signature
  in a JWT so long as the algorithm used is supported by the JWK retrieved from
  the URL.

## [v2025.02.07]

### Added

- Added type checking for object-type values in field presets within type
  permissions and for argument presets in model or command permissions.
- Added a OpenDD flag
  (`disallow_local_relationships_on_data_connectors_without_relationships_or_variables`)
  to disallow local relationships on data connectors without the `relationships`
  and `variables` capabilities.

### Fixed

- Invalid JSON pointers provided in AuthConfig's `jwt.claimsConfig.locations`
  now raise a build error, whereas previously they were silently ignored.
- Orderable relationships in OrderByExpressions will now produce a build error
  if the relationship is a remote relationship, or if the data connector does
  not support relationships, or if the target of the relationship does not have
  a source.
- Fixed a bug causing queries to fail when filter expressions included remote
  relationships from nested fields.

### Changed

## [v2025.02.03]

### Added

- Allow environment variables to be used in permission filters.
- Added a OpenDD flag (`disallow_order_by_fields_with_field_arguments`) to
  disallow fields with field arguments in order by expressions

### Fixed

- Reduce errors for unnecessary GraphQL configuration in aggregates to warnings

- The `args` field for a model is now optional if all arguments are nullable or
  preset.

Previously, this would be necessary:

```graphql
query MyQuery {
  customers(args: {}) {
    # `args` is mandatory, even if it is empty
    email
  }
}
```

Now, if all fields are nullable or preset, this is acceptable:

```graphql
query MyQuery {
  customers {
    # `args` is no longer mandatory
    email
  }
}
```

### Changed

- Check that all named types referenced in object type fields exist.

## [v2025.01.24]

### Added

- Added configuration for headers in response to client for requests handled by
  the pre-route plugin hook.
- Added OpenAPI schema for filter parameters in JSONAPI
- Added checks that the fields of an `ObjectType` match the underlying column
  types exposed by the data connector.

### Fixed

- Fixed a bug where TypePermissions field presets would be ignored if the type
  was inside a nested array.
- Fixed a bug where TypePermissions field presets would be ignored if recursive
  types were used.

## [v2025.01.17]

### Added

#### Pre-route Engine Plugins

Add support for pre-route engine plugins. Engine now supports calling a HTTP
webhook in pre-route execution step. This can be used to add a bunch of
functionalities to the DDN, such as a restify middleware, etc.

The following is an example of the OpenDD metadata for the plugins:

```yaml
kind: LifecyclePluginHook
version: v1
definition:
  pre: route
  name: restified_endpoints
  url:
    value: http://localhost:5001/restified
  config:
    match: "/v1/api/rest/*"
    matchMethods: ["GET", "POST"]
    request:
      method: POST
      headers:
        forward:
          - Authorization
        additional:
          hasura-m-auth:
            value: "your-strong-m-auth-key"
      rawRequest:
        path: {}
        query: {}
        method: {}
        body: {}
```

This will match all GET and POST requests to `/v1/api/rest/*` and make a POST
request to `http://localhost:5001/restified` with the following request body:

```json
{
  "path": "/v1/api/rest/some/path",
  "method": "GET",
  "query": "some=query&params=here",
  "body": "..."
}
```

Additionally, the following headers will be sent with the request:

- `Authorization` header from the original request
- `hasura-m-auth` header with the value `your-strong-m-auth-key`

The response from the plugin will be used as the response to the original
request.

The pre-route plugin hook's request can be customized using the
`LifecyclePluginHook` metadata object. Currently we support the following
customizations:

- forwarding/additional headers
- adding/removing path information
- adding/removing query information
- adding/removing method information
- adding/removing body information

### Fixed

- Allow reusing `OrderByExpressions` in different models.
- Fixed a bug where erroneous build errors would be raised when using a
  relationship in a boolean expression pre-set to an argument in
  CommandPermissions.
- Fixed a bug where setting both Command/Model argument presets (via
  ModelPermissions or CommandPermissions) as well as setting field presets (via
  TypePermissions) would result in the argument presets being ignored.
- Fixed a bug where setting field presets (via TypePermissions) on multiple
  types that were nested inside each other would result in some field presets
  being ignored.
- Fixed a bug where TypePermissions field presets would be ignored if the
  argument type was an array of objects.
- Fixed a bug where a successful response from a pre-parse plugin would have a
  Content-Type header `application/octet-stream`. Now it returns
  `Content-Type: application/json`

## [v2025.01.09]

### Fixed

- Fixed a bug where erroneous typechecking errors could be produced when
  querying a Model or Command that used an object type as an argument that had
  input field presets applied on its TypePermissions.

## [v2025.01.06]

### Added

- Added support for relationships targeting commands in JSON:API.

### Fixed

- Fix syntax error in descriptions in generated SDL, when description strings
  contained a trailing double-quote
- Filtering by fields in an object nested inside an array that itself is nested
  inside an object now works correctly. Previously this would generate an
  invalid query.
- Prevent conflicting names for `BooleanExpressionType`, `OrderByExpression`,
  `ScalarType` and `ObjectType` definitions.

## [v2024.12.17]

### Added

- Plugins will now receive the `X-FORWARDED-FOR` header, which contains the IP
  address of the client who made the request. This allows for the implementation
  of plugins such as IP allow lists and per-user traffic control.

- `count` and `countDistinct` in `AggregateExpression`s now support a
  `resultType` field. This allows you to specify the type of the result of the
  count aggregation. Currently, only `Int` is supported, but this will change in
  the future once data connector support arrives. Omitting this field will
  default to `Int`.

### Fixed

- Fixed the `include` query parameter and `included` response field in JSON:API
  OpenAPI schema generation. These now honor type permissions for the role in
  relationship fields.

- Fixed a bug where commands with array return types would not build when header
  forwarding was in effect.

- GraphQL queries with `order_by` arguments that contain multiple properties set
  on one input object now properly return an error. For example
  `order_by: { location: { city: Asc, country: Asc } }` is no longer allowed.
  This is because the order of input object fields in GraphQL is not defined, so
  it is unclear whether ordering should be first by city or first by country.
  Instead, write this query like so:
  `order_by: [{ location: { city: Asc } }, { location: { country: Asc } }]`.

  Additionally, ordering by nested fields using an nested array is no longer
  allowed (for example:
  `order_by: { location: [{ city: Asc }, { country: Asc }] }`). Instead, write
  this query like so:
  `order_by: [{ location: { city: Asc } }, { location: { country: Asc } }]`.

  These fixes are only enabled if your `CompatibilityConfig` date is set to
  `2024-12-10` or newer.

- Fixed a bug where `orderableFields` in a `Model` `v1` would allow you to
  attempt to order by non-scalar fields. At runtime, queries would either fail
  or produce unexpected results. Trying to use a non-scalar `orderableField`
  will now result in a build error. However, it is recommended you upgrade to
  `Model` `v2` and use `OrderByExpression` instead.

- Fixed a bug where `OrderByExpression`s that incorporate nested fields and
  nested relationships were allowed to be used with `Model`s that source from
  data connectors that did not support ordering by nested fields and nested
  relationships. Such a configuration will now result in a build error.

- Fixed a bug where `OrderByExpressions` could refer to array relationships in
  an `orderableRelationship`. Such a configuration will now result in a build
  error.

## [v2024.12.04]

### Added

- Added support for the sparse fieldset parameter for nested field types in
  JSON:API.

### Fixed

- Conflicts between `BooleanExpressionType` fields that have the same name are
  now detected and a build error is raised. Previously the duplicated fields
  would have been silently dropped.
- Row filters configured in `ModelPermissions` are now correctly applied when
  referencing the model across a relationship in a filter predicate.
- Row filters configured in `ModelPermissions` are now correctly applied when
  referencing the model across a relationship is order by expressions.

## [v2024.11.25]

### Added

#### Logical operators in scalar boolean expressions

Adds the ability to use `_and`, `_or` and `_not` operators at every level of the
where clause in GraphQL queries. Previously logical operators only appeared at
the object type level of expressions, not at the scalar type level.

Instead of writing:

```graphql
query {
  AuthorMany(
    where: { _or: [{ author_id: { _eq: 1 } }, { author_id: { _eq: 2 } }] }
  ) {
    author_id
    first_name
  }
}
```

You can now write:

```graphql
query {
  AuthorMany(where: { author_id: { _or: [{ _eq: 1 }, { _eq: 2 }] } }) {
    author_id
    first_name
  }
}
```

In order to use this, you must have `logicalOperators` enabled on your scalar
`BooleanExpressionType` and your
[compatibility date](https://hasura.io/docs/3.0/supergraph-modeling/compatibility-config/)
must be at least `2024-11-26`.

#### Other

- Added support for fetching relationships in JSON:API using the `include`
  parameter.

### Fixed

- Fixed an error that occurred when filtering by more than one nested field at a
  time.
- Fixed an error that occurred when filtering using logical operators (eg
  `_and`) from inside a nested field.

### Changed

- MBS error contexts now contain explicit subgraphs where appropriate for each
  individual error step.

## [v2024.11.18]

### Added

#### Subscriptions

Adds real-time data capabilities through GraphQL subscriptions. Available for a
model's select unique, select many, and aggregate queries.

For fresh DDN projects, subscription support is automatically enabled when
adding models via `ddn model add`. For existing projects, run
`ddn codemod upgrade-graphqlconfig-subscriptions` to enable.

Configure
[polling intervals](https://hasura.io/docs/3.0/graphql-api/subscriptions/#polling-interval)
in your model's metadata (defaults to 1000ms). Use the `allowSubscriptions` flag
in
[select permissions](https://hasura.io/docs/3.0/graphql-api/subscriptions/#permissions)
to control role access.

Example subscription:

```graphql
subscription UserNotificationSubscription {
  notifications(where: { user_id: { _eq: 123 } }) {
    id
    created_at
    message
  }
}
```

For more details, see the
[subscriptions documentation](https://hasura.io/docs/3.0/graphql-api/subscriptions/).

## [v2024.11.13]

Minor internal refactors

## [v2024.11.11]

### Changed

- Prevent duplicate GraphQL root field names for models; previously, one was
  arbitrarily chosen as the exposed field. This change does not impact existing
  projects, and warnings are emitted while creating a build. Projects created or
  with a
  [compatibility date](https://hasura.io/docs/3.0/supergraph-modeling/compatibility-config/)
  after `2024-11-15` are affected.

## [v2024.11.05]

### Added

#### JSONAPI alpha release

Adds a new set of endpoints at `/v1/rest` that follow the
[JSONAPI](https://jsonapi.org/) specification.

An [OpenAPI](https://swagger.io/specification/) schema for a given role can be
accessed at `v1/rest/__schema`.

Currently, every model that a given role is able to access is exposed at
`GET v1/rest/subgraph/model`. In further releases models will be explicitly
configured and exposed via metadata to match the GraphQL schema.

Select the fields you receive with `?fields[model]=fieldname,anotherfield`.

Limit the number of results with `?page[limit]=10`

Offset the results with `?page[offset]=5`

Order the results with `?sort[model]=fieldname,-anotherfield`. Default is
sorting in ascending order, adding `-` at the start of the field name makes the
ordering descending instead.

This feature is still very much alpha and in active development, all feedback
gratefully received.

## [v2024.10.30]

### Added

#### Order by Nested Fields

Add support for ordering by nested fields.

Example query:

```graphql
query MyQuery {
  InstitutionMany(order_by: { location: { city: Asc } }) {
    id
    location {
      city
      campuses
    }
  }
}
```

This will order by the value of the nested field `city` within the `location`
column.

- New metadata item `OrderByExpression`

- New metadata item `Model` version 2

### Fixed

### Changed

## [v2024.10.25]

### Fixed

- `ModelPermissions` and `CommandPermissions` can now correctly preset
  predicate-typed arguments that are nullable. Previously, trying to do so
  resulted in a build error.

## [v2024.10.23]

### Added

- Add a check to disallow defining boolean expression of array fields with
  scalar boolean type while resolving the boolean expression

### Fixed

- Improve performance of metadata builds

- When the `CompatibilityConfig` date is set to `2024-10-16` or newer, session
  variables returned by webhooks, set in `noAuth` config in `AuthConfig` or set
  in JWT claims are now correctly allowed to be full JSON values, not just JSON
  strings. This fixes the bug where you were incorrectly required to JSON-encode
  your JSON value inside a string. For example, you were previously incorrectly
  required to return session variables like this
  `{ "X-Hasura-AllowedUserIds": "[1,2,3]" }`, but now you can correctly return
  them like this: `{ "X-Hasura-AllowedUserIds": [1,2,3] }`.

- The warning about AuthConfig v1 being deprecated was only being displayed in
  the engine's stdout logs and not as a build warning. This has been corrected.

## [v2024.10.21]

### Added

- Support array values in session variables

### Fixed

### Changed

## [v2024.10.14]

### Added

- Added contexts to more MBS errors: when a model refers to a collection that
  doesn't exist, the path to the offending reference will be reported.

### Fixed

- Fix local `docker-compose.yaml` file so that running `docker compose up`
  builds the engine and serves it along with a sample schema using
  `ndc-postgres` and a `postgres` database.

- Subgraph builds that have relationships to other external subgraphs can now be
  run locally and no longer fail with missing subgraph errors. Subgraph builds
  are marked with a new OpenDD flag and when these builds are run by the engine
  relationships to unknown subgraphs are automatically pruned.

- Aggregate queries now support `__typename` introspection fields.

### Changed

- metadata-build-service POST endpoints now accept zstd (preferred) or gzip
  -encoded request bodies

- The `--partial-supergraph` command-line argument and `PARTIAL_SUPERGRAPH`
  environment variable have been removed. Builds now contain an OpenDD flag that
  indicates if they are subgraph builds and should be run as such.

## [v2024.10.02]

### Added

#### Metadata build error contexts

Contexts are being added to errors raised during the build process to allow
users to locate the source of the issue more quickly. These contexts will be
surfaced in the Build Server API responses. The first example and test bed for
developing the scaffolding is the error raised when a model refers to a
nonexistent data connector. This error will now also contain the path to the
offending data connector name.

#### Pre-response Plugin

Engine now supports calling a HTTP webhook in the pre-response execution step.
This can be used to add some post execution functionalities to the DDN, such as
sending the response to a logging service, sending notifications for specific
requests like mutations, etc.

The following is an example of the OpenDD metadata for the pre-response plugin:

```yaml
kind: LifecyclePluginHook
version: v1
definition:
  name: logging
  url:
    value: http://localhost:5001/log
  pre: response
  config:
    request:
      headers:
        additional:
          hasura-m-auth:
            value: "your-strong-m-auth-key"
      session: {}
      rawRequest:
        query: {}
        variables: {}
      rawResponse: {}
```

Similar to the pre-parse plugin, the pre-response plugin's request can be
customized using the `LifecyclePluginHook` metadata object. Currently we support
the following customizations:

- adding/removing session information
- adding new headers
- forwarding specific headers
- adding/removing graphql query and variables
- adding/removing response

### Fixed

- Fix poor performance of `process_response` for large and deeply-nested results
- Fixed issue in partial supergraph builds where a `BooleanExpressionType` that
  referenced a relationship that targeted an unknown subgraph would incorrectly
  produce an error rather than ignoring the relationship
- Fixed double string escaping when forwarding headers to a data connector

### Changed

- Making `args` non-compulsory for models where all arguments have presets.

Previously, if a model had arguments specified that were all provided by
presets, then we would require them to pass an empty `args: {}` argument:

```graphql
query MyQuery
  ActorsByMovieMany(args: {}) {
    actor_id
    movie_id
    name
  }
}
```

This change loosens the restriction, so now the following query is valid too:

```graphql
query MyQuery
  ActorsByMovieMany {
    actor_id
    movie_id
    name
  }
}
```

- OpenTelemetry service name set to `ddn-engine` to avoid confusion with
  `graphql-engine`.

- Builds can no longer contain two commands with the same root field name.
  Previously, one of the two commands would be chosen arbitrarily as the exposed
  root field. Now, this raises a build-time error.

## [v2024.09.23]

### Fixed

- Disallow defining custom scalar types with names that conflict with built-in
  types, such as `String` or `Int`.

- Fixed bug where relationships defined on a boolean expression would not take
  the target subgraph into account.

- Propagate deprecation status to boolean expression relationship fields.

## [v2024.09.16]

### Fixed

- Raise a warning when nested array comparisons are used without the necessary
  data connector capability. A new OpenDD flag
  `require_nested_array_filtering_capability` can be used to promote this
  warning to an error.

- Disallow recursive types in SQL table column types.

- Previously, if you had `AggregateExpressions` that were configured to be used
  in GraphQL, or `Models` configured for aggregates in GraphQL, but you did not
  set the appropriate configuration in
  `GraphqlConfig.definition.query.aggregates`, the build would fail with an
  error. This has been relaxed so that the build now succeeds, but warnings are
  raised instead. However, the aggregates will not appear in your GraphQL API
  until the `GraphqlConfig` is updated. This allows you to add
  `AggregateExpressions` and configure your `Model` but update your
  `GraphqlConfig` separately, which is useful if they are in separate
  repositories.

- A build error is now raised if an `AggregateExpression` specifies an
  `aggregatableField` that has field arguments. This is an unsupported scenario
  and previously would have allowed invalid queries that omitted the required
  field arguments. These queries may have failed with errors at query time.

- Add a missing typecheck of `ValueExpression` while resolving model predicates.

## [v2024.09.05]

### Added

- SQL endpoint can utilize uniqueness constraints

### Fixed

- Fix the name and description of the span resolving relationship predicates in
  the Engine.

### Changed

## [v2024.09.02]

### Added

- Enhanced handling of relationships in predicates
- Filter nested arrays
- Order by nested fields
- A new GraphQL config flag `require_valid_ndc_v01_version` to promote warnings
  about NDC version as errors.

#### Enhanced Handling of Relationships in Predicates

Improved support for using relationships in boolean expressions even when the
data connector lacks the `relation_comparisons` capability. This update
introduces two strategies for handling relationship predicates:

- **Data Connector Pushdown**: When the source and target connectors are the
  same and the target connector supports relationship comparisons, predicates
  are pushed down to the NDC (Data Connector) for more efficient processing.
  This strategy optimizes query execution by leveraging the data connector’s
  capabilities.

- **Engine-Based Resolution**: When the data connector does not support
  relationship comparisons or when dealing with relationships targeting models
  from other data connectors (remote relationships), predicates are resolved
  internally within the engine. This approach involves querying the target
  model’s field values and constructing the necessary comparison expressions.

This enhancement updates the GraphQL schema's boolean expression input types by
introducing relationship predicates. The feature is gated by a compatibility
date to ensure backward compatibility. To enable it, set the date to
`2024-09-03` or later in your DDN project's `globals/compatibility-config.hml`
file.

#### Filter Nested Arrays

If `institution` is a big JSON document, and `staff` is an array of objects
inside it, we can now filter `institutions` based on matches that exist within
that array.

```graphql
query MyQuery {
  where_does_john_hughes_work: InstitutionMany(
    where: { staff: { last_name: { _eq: "Hughes" } } }
  ) {
    id
    location {
      city
      campuses
    }
  }
```

This query would return us details of `Chalmers University of Technology`, where
`John Hughes` is a member of staff.

### Fixed

- Stack overflow error on startup. Even if the (experimental) SQL feature was
  turned off, engine would try to build a SQL catalog on startup. Now it will
  build an empty catalog.

### Changed

## [v2024.08.22]

### Added

#### Pre-parse Engine Plugins

Add support for pre-parse engine plugins. Engine now supports calling a HTTP
webhook in pre-parse execution step. This can be used to add a bunch of
functionalities to the DDN, such as an [allow list][plugin-allowlist].

The following is an example of the OpenDD metadata for the plugins:

```yaml
kind: LifecyclePluginHook
version: v1
definition:
name: allow list
url: http://localhost:8787
pre: parse
config:
  request:
    headers:
      additional:
      hasura-m-auth:
        value: "your-strong-m-auth-key"
    session: {}
    rawRequest:
      query: {}
      variables: {}
```

The pre-parse plugin hook's request can be customized using the
`LifecyclePluginHook` metadata object. Currently we support the following
customizations:

- adding/removing session information
- adding new headers
- forwarding specific headers
- adding/removing graphql query and variables

### Fixed

- Disallow model filter boolean expressions having relationship comparisons in
  their nested object filters.

### Changed

[plugin-allowlist]: https://github.com/hasura/plugin-allowlist

## [v2024.08.07]

### Added

- A new CLI flag (`--export-traces-stdout`) and env var (`EXPORT_TRACES_STDOUT`)
  is introduced to enable logging of traces to STDOUT. By default, logging is
  disabled.

#### Remote Relationships Predicates

We have significantly enhanced our permission capabilities to support remote
relationships in filter predicates. It is important to note that the
relationship source and target models should be from the same subgraph.

**Example:** API traces are stored in a separate database. Users should only be
able to view traces of their own API requests.

```yaml
kind: ModelPermissions
version: v1
definition:
  modelName: traces
  permissions:
    - role: user
      select:
        filter:
          relationship:
            name: User
            predicate:
              fieldComparison:
                field: user_id
                operator: _eq
                value:
                  sessionVariable: x-hasura-user-id
```

In the above configuration, a permission filter is defined on the `traces`
model. The filter predicate employs the `User` remote relationship, ensuring the
`user_id` field is equal to the `x-hasura-user-id` session variable.

- New `NoAuth` mode in auth config can be used to provide a static role and
  session variables to use whilst running the engine, to make getting started
  easier.

### Fixed

- Fixes a bug where queries with nested relationship selection and filter
  predicates fail due to an issue with NDC relationship collection

- Reduce error for using nested arrays in boolean expressions to a warning to
  maintain backwards compatibility

- Fix use of object types as comparison operator arguments by correctly
  utilising user-provided OpenDD types.

- Fixes a bug where argument presets set in the DataConnectorLink were sent to
  every connector function/procedure regardless of whether the
  function/procedure actually declared that argument

- Fixes a bug where argument presets set in the DataConnectorLink were not sent
  to connector collections that backed Models

- Fixes a bug where the type of the argument name in the DataConnectorLink's
  argument presets was incorrect in the Open DD schema. It was `ArgumentName`
  but should have been `DataConnectorArgumentName`

- Fixes a bug where the check to ensure that argument presets in the
  DataConnectorLink does not overlap with arguments defined on Models/Commands
  was comparing against the Model/Command argument name not the data connector
  argument name

### Changed

- Introduced `AuthConfig` `v2`. This new version removes role emulation in
  engine (`allowRoleEmulationBy`) field.

- Raise a warning when an invalid data connector capabilities version is used in
  in a `DataConnectorLink` and prevent the usage of incompatible data connector
  capabilities versions

- Models and commands that do not define all the necessary arguments to satisfy
  the underlying data connector collection/function/procedure now cause warnings
  to be raised. The warnings will be turned into errors in the future.

## [v2024.07.25]

### Fixed

- Ensured `traceresponse` header is returned

## [v2024.07.24]

### Added

- The metadata resolve step now emits warnings to let users know about
  soon-to-be deprecated features and suggest fixes.

### Fixed

- Fixes a bug where boolean expressions passed as arguments would not be
  translated into NDC `Expression` types before being sent to the data
  connector.

- Fixes a bug where relationships within nested columns would throw an internal
  error. While generating NDC relationship definitions, engine would ignore
  columns with nested selection.

- Renamed the `ArgumentPreset` for data connectors to
  `DataConnectorArgumentPreset` to avoid ambiguity in generated JSONSchema.

### Changed

- Fixed a bug where command targeted relationships were not using the Open DD
  argument name instead of the data connector's argument name when querying the
  data connector

## [v2024.07.18]

### Added

#### Remote Relationships in Query Filter

We have enhanced the GraphQL query capabilities to support array and object
relationships targeting models backed by different data connectors. This allows
you to specify remote relationships directly within the `where` expression of
your queries.

**Example:** Retrieve a list of customers who have been impacted by cancelled
orders during the current sale event. This data should be filtered based on
order logs stored in a separate data source.

```graphql
query CustomersWithFailedOrders {
  Customers(
    where: {
      OrderLogs: {
        _and: [
          { timestamp: { _gt: "2024-10-10" } }
          { status: { _eq: "cancelled" } }
        ]
      }
    }
  ) {
    CustomerId
    EmailId
    OrderLogs {
      OrderId
    }
  }
}
```

By incorporating remote relationships into the where expression, you can
seamlessly query and filter data that spans across multiple data sources, making
your GraphQL queries more versatile and powerful.

### Fixed

- Build-time check to ensure boolean expressions cannot be built over nested
  array fields until these are supported.

- Fixed a bug where command targeted relationships were not using the OpenDD
  argument name instead of the data connector's argument name when querying the
  data connector.

## [v2024.07.10]

### Fixed

- Fixes a bug with variable nullability coercion. Specifically, providing a
  non-null variable for a nullable field should work, as all non-nullable
  variables can be used as nullable variables via "coercion".

- Fixes a bug where data connectors without the `foreach` capability were not
  allowed to create local relationships

## [v2024.07.04]

### Added

- Query Usage Analytics - usage analytics JSON data is attached to `execute`
  span using `internal.query_usage_analytics` attribute

- Added a flag, `--partial-supergraph`, which instructs the metadata resolver to
  prune relationships to unknown subgraphs rather than failing to resolve

#### Boolean Expression Types

A new metadata kind `BooleanExpressionType` can now be defined. These can be
used in place of `ObjectBooleanExpressionType` and
`DataConnectorScalarRepresentation`, and allow more granular control of
comparison operators and how they are used.

```yaml
kind: BooleanExpressionType
version: v1
definition:
  name: album_bool_exp
  operand:
    object:
      type: Album
      comparableFields:
        - fieldName: AlbumId
          booleanExpressionType: pg_int_comparison_exp
        - fieldName: ArtistId
          booleanExpressionType: pg_int_comparison_exp_with_is_null
        - field: Address
          booleanExpressionType: address_bool_exp
      comparableRelationships:
        - relationshipName: Artist
          booleanExpressionType: artist_bool_exp
  logicalOperators:
    enable: true
  isNull:
    enable: true
  graphql:
    typeName: app_album_bool_exp
```

```yaml
kind: BooleanExpressionType
version: v1
definition:
  name: pg_int_comparison_exp
  operand:
    scalar:
      type: Int
      comparisonOperators:
        - name: equals
          argumentType: String!
        - name: _in
          argumentType: [String!]!
      dataConnectorOperatorMapping:
        - dataConnectorName: postgres_db
          dataConnectorScalarType: String
          operatorMapping:
            equals: _eq
  logicalOperators:
    enable: true
  isNull:
    enable: true
  graphql:
    typeName: app_postgres_int_bool_exp
```

- Add flag to (`--expose-internal-errors`) toggle whether to expose internal
  errors. ([#759](https://github.com/hasura/v3-engine/pull/759))

#### Aggregates of Array Relationships

Aggregates of array relationships can now be defined by specifying an
`aggregate` in the `Relationship`'s target. Note that this is only supported
when the target of the relationship is a `Model`. You must also specify the
`aggregateFieldName` under the `graphql` section.

```yaml
kind: Relationship
version: v1
definition:
  name: invoices
  sourceType: Customer
  target:
    model:
      name: Invoice
      relationshipType: Array
      aggregate: # New!
        aggregateExpression: Invoice_aggregate_exp
        description: Aggregate of the customer's invoices
  mapping:
    - source:
        fieldPath:
          - fieldName: customerId
      target:
        modelField:
          - fieldName: customerId
  graphql: # New!
    aggregateFieldName: invoicesAggregate
```

- One can now configure the engine to set response headers for GraphQL requests,
  if NDC function/procedures returns headers in its result

#### Field arguments

Add field arguments to the OpenDD `ObjectType`:

```yaml
kind: ObjectType
version: v1
definition:
  name: institution
  fields:
    - name: id
      type: Int!
    - name: name
      type: String!
      arguments:
        - name: hash
          argumentType: String
        - name: limit
          argumentType: Int
        - name: offset
          argumentType: Int
  graphql:
    typeName: Institution
  dataConnectorTypeMapping:
    - dataConnectorName: custom
      dataConnectorObjectType: institution
      fieldMapping:
        id:
          column:
            name: id
        name:
          column:
            name: name
            argumentMapping:
              hash: hash
              offset: offset
              limit: limit
```

### Changed

### Fixed

- Engine now respects `relation_comparisons` capability, when generating GraphQL
  schema for relationship fields in model filter
- The OpenDD schema for `DataConnectorLink` now references the correct version
  (v0.1.4) of the NDC schema when using the NDC `CapabilitiesResponse` and
  `SchemaResponse` types

## [v2024.06.13]

Initial release.

<!-- end -->

[Unreleased]: https://github.com/hasura/v3-engine/compare/v2025.08.18...HEAD
[v2025.08.18]: https://github.com/hasura/v3-engine/releases/tag/v2025.08.18
[v2025.08.14-1]: https://github.com/hasura/v3-engine/releases/tag/v2025.08.14-1
[v2025.08.14]: https://github.com/hasura/v3-engine/releases/tag/v2025.08.14
[v2025.08.13]: https://github.com/hasura/v3-engine/releases/tag/v2025.08.13
[v2025.07.29]: https://github.com/hasura/v3-engine/releases/tag/v2025.07.29
[v2025.07.28]: https://github.com/hasura/v3-engine/releases/tag/v2025.07.28
[v2025.07.22]: https://github.com/hasura/v3-engine/releases/tag/v2025.07.22
[v2025.07.21]: https://github.com/hasura/v3-engine/releases/tag/v2025.07.21
[v2025.07.14]: https://github.com/hasura/v3-engine/releases/tag/v2025.07.14
[v2025.07.10]: https://github.com/hasura/v3-engine/releases/tag/v2025.07.10
[v2025.07.07]: https://github.com/hasura/v3-engine/releases/tag/v2025.07.07
[v2025.07.02]: https://github.com/hasura/v3-engine/releases/tag/v2025.07.02
[v2025.06.27]: https://github.com/hasura/v3-engine/releases/tag/v2025.06.27
[v2025.06.26]: https://github.com/hasura/v3-engine/releases/tag/v2025.06.26
[v2025.06.16]: https://github.com/hasura/v3-engine/releases/tag/v2025.06.16
[v2025.06.04]: https://github.com/hasura/v3-engine/releases/tag/v2025.06.04
[v2025.05.29]: https://github.com/hasura/v3-engine/releases/tag/v2025.05.29
[v2025.05.14]: https://github.com/hasura/v3-engine/releases/tag/v2025.05.14
[v2025.05.13]: https://github.com/hasura/v3-engine/releases/tag/v2025.05.13
[v2025.04.30]: https://github.com/hasura/v3-engine/releases/tag/v2025.04.30
[v2025.04.28]: https://github.com/hasura/v3-engine/releases/tag/v2025.04.28
[v2025.04.23]: https://github.com/hasura/v3-engine/releases/tag/v2025.04.23
[v2025.04.14]: https://github.com/hasura/v3-engine/releases/tag/v2025.04.14
[v2025.04.02]: https://github.com/hasura/v3-engine/releases/tag/v2025.04.02
[v2025.03.25]: https://github.com/hasura/v3-engine/releases/tag/v2025.03.25
[v2025.03.20]: https://github.com/hasura/v3-engine/releases/tag/v2025.03.20
[v2025.03.17]: https://github.com/hasura/v3-engine/releases/tag/v2025.03.17
[v2025.03.13]: https://github.com/hasura/v3-engine/releases/tag/v2025.03.13
[v2025.03.11]: https://github.com/hasura/v3-engine/releases/tag/v2025.03.11
[v2025.03.10]: https://github.com/hasura/v3-engine/releases/tag/v2025.03.10
[v2025.02.26]: https://github.com/hasura/v3-engine/releases/tag/v2025.02.26
[v2025.02.20]: https://github.com/hasura/v3-engine/releases/tag/v2025.02.20
[v2025.02.19]: https://github.com/hasura/v3-engine/releases/tag/v2025.02.19
[v2025.02.07]: https://github.com/hasura/v3-engine/releases/tag/v2025.02.07
[v2025.02.03]: https://github.com/hasura/v3-engine/releases/tag/v2025.02.03
[v2025.01.24]: https://github.com/hasura/v3-engine/releases/tag/v2025.01.24
[v2025.01.17]: https://github.com/hasura/v3-engine/releases/tag/v2025.01.17
[v2025.01.09]: https://github.com/hasura/v3-engine/releases/tag/v2025.01.09
[v2025.01.06]: https://github.com/hasura/v3-engine/releases/tag/v2025.01.06
[v2024.12.17]: https://github.com/hasura/v3-engine/releases/tag/v2024.12.17
[v2024.12.03]: https://github.com/hasura/v3-engine/releases/tag/v2024.12.03
[v2024.11.25]: https://github.com/hasura/v3-engine/releases/tag/v2024.11.25
[v2024.11.18]: https://github.com/hasura/v3-engine/releases/tag/v2024.11.18
[v2024.11.13]: https://github.com/hasura/v3-engine/releases/tag/v2024.11.13
[v2024.11.11]: https://github.com/hasura/v3-engine/releases/tag/v2024.11.11
[v2024.11.05]: https://github.com/hasura/v3-engine/releases/tag/v2024.11.05
[v2024.10.30]: https://github.com/hasura/v3-engine/releases/tag/v2024.10.30
[v2024.10.25]: https://github.com/hasura/v3-engine/releases/tag/v2024.10.25
[v2024.10.23]: https://github.com/hasura/v3-engine/releases/tag/v2024.10.23
[v2024.10.21]: https://github.com/hasura/v3-engine/releases/tag/v2024.10.21
[v2024.10.14]: https://github.com/hasura/v3-engine/releases/tag/v2024.10.14
[v2024.10.02]: https://github.com/hasura/v3-engine/releases/tag/v2024.10.02
[v2024.09.23]: https://github.com/hasura/v3-engine/releases/tag/v2024.09.23
[v2024.09.16]: https://github.com/hasura/v3-engine/releases/tag/v2024.09.16
[v2024.09.05]: https://github.com/hasura/v3-engine/releases/tag/v2024.09.05
[v2024.09.02]: https://github.com/hasura/v3-engine/releases/tag/v2024.09.02
[v2024.08.07]: https://github.com/hasura/v3-engine/releases/tag/v2024.08.07
[v2024.07.25]: https://github.com/hasura/v3-engine/releases/tag/v2024.07.25
[v2024.07.24]: https://github.com/hasura/v3-engine/releases/tag/v2024.07.24
[v2024.07.18]: https://github.com/hasura/v3-engine/releases/tag/v2024.07.18
[v2024.07.10]: https://github.com/hasura/v3-engine/releases/tag/v2024.07.10
[v2024.07.04]: https://github.com/hasura/v3-engine/releases/tag/v2024.07.04
[v2024.06.13]: https://github.com/hasura/v3-engine/releases/tag/v2024.06.13
