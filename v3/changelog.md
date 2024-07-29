# Changelog

## [Unreleased]

### Added

- New `NoAuth` mode in auth config can be used to provide a static role and
  session variables to use whilst running the engine, to make getting started
  easier.

### Fixed

### Changed

- Introduced `AuthConfig` `v2`. This new version removes role emulation in
  engine (`allowRoleEmulationBy`) field.

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

[Unreleased]: https://github.com/hasura/v3-engine/compare/v2024.07.25...HEAD
[v2024.07.25]: https://github.com/hasura/v3-engine/releases/tag/v2024.07.25
[v2024.07.24]: https://github.com/hasura/v3-engine/releases/tag/v2024.07.24
[v2024.07.18]: https://github.com/hasura/v3-engine/releases/tag/v2024.07.18
[v2024.07.10]: https://github.com/hasura/v3-engine/releases/tag/v2024.07.10
[v2024.07.04]: https://github.com/hasura/v3-engine/releases/tag/v2024.07.04
[v2024.06.13]: https://github.com/hasura/v3-engine/releases/tag/v2024.06.13
