# Changelog

## [Unreleased]

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

[Unreleased]: https://github.com/hasura/v3-engine/compare/v2024.06.13...HEAD
[v2024.06.13]: https://github.com/hasura/v3-engine/releases/tag/v2024.06.13
[v2024.07.04]: https://github.com/hasura/v3-engine/releases/tag/v2024.07.04
[v2024.07.10]: https://github.com/hasura/v3-engine/releases/tag/v2024.07.10
