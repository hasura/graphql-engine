# Changelog

## [Unreleased]

### Added

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
JSONB column.

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

[Unreleased]: https://github.com/hasura/v3-engine/compare/v2024.10.25...HEAD
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
