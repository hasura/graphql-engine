# Open DD expression type changes

## Motivation

Currently the boolean expression and order by expressions are defined as
follows:

### DataConnectorScalarRepresentation

Boolean expressions for scalars are implicitly defined by the comparison
operators of the NDC schema. The `DataConnectorScalarRepresentation` defines the
following properties:

- When a scalar type appears as the argument type of a comparison operator, what
  OpenDD type to use via the `representation` field.
- What is the graphql type name to use for this scalar type's comparison
  expression.

There are a few problems with this:

- You would expect that the OpenDD `representation` of a scalar type applies
  everywhere it shows up, but in the context of object fields or arguments it's
  controlled by the `type` of the field/argument definition. This can actually
  differ from what the scalar type representation says.
- If the argument type of a comparison operator is a complex object type, there
  is no way to know what OpenDD type to use, since we define this representation
  only for scalars.
- To allow for scalar types from different data connectors deployments (but the
  same underlying type of data connector, e.g. postgres) to be able to share the
  same comparison expression types, the engine subtly allows defining duplicate
  graphql type names as long as the comparison operators are identical. Ideally,
  this would be an explicit definition.
- There need to be multiple graphql types for the scalar comparison expression
  depending on which operators are enabled. Supporting this within the current
  definition would involve defining a graphql type name per-combination of
  operators which is awkward.

### ObjectBooleanExpressionType

The `ObjectBooleanExpressionType` configures a boolean expression on an object
type. It lets the user define which fields of the object are comparable and
which scalar comparison operators to enable for each field. This has the
following problems:

- Fields don't have to have scalar types, they can have object types too which
  isn't handled by the current definition.
- You cannot control the behavior of the `is_null` operation which isn't a
  regular comparison operator defined on a scalar type.
- You can't control which relationships can be used for comparison.

### Model orderableFields

The `orderableFields` field of a `Model` configure the order by expression for
that model. This has similar problems to `ObjectBooleanExpressionType`.

- Fields that have object types aren't handled at all.
- You can't control which relationships can be used for ordering.

## Proposal

This RFC proposes the following changes:

- Remove the existing constructs for configuring these expressions completely.
- Add a common `kind: BooleanExpressionType` that works for both scalar types
  and object types.
- Do not tie boolean expressions to a particular NDC type, but instead define
  them purely in OpenDD terms and then map them to NDC scalar types / operators
  (similar to how we map `Model`s and `ObjectType`s). This allows flexible
  composition and explicit reuse of boolean expressions.
- Add a `kind: OrderByExpression` which is used to configure order by
  expressions. This will allow supporting nested order by expressions. Note that
  this is not an OpenDD type and cannot be used as a field/argument type.

### BooleanExpressionType

#### Scalar boolean expressions

```yaml
kind: BooleanExpressionType
version: v1
definition:
  name: Int_comparison_exp_with_eq_within_and_is_null
  operand:
    scalar:
      type: Int
      comparisonOperators:
        - name: _eq # Name you want to give the operator in OpenDD / graphql
          argumentType: Int! # This is an OpenDD type
        - name: _within
          argumentType: WithinInput!
        - name: _in
          argumentType: "[Int!]!"
      dataConnectorOperatorMapping:
        - dataConnectorName: pg_1
          dataConnectorScalarType: int8
          operatorMapping:
            _within: int_within
            _eq: _eq
        - dataConnectorName: pg_2
          dataConnectorScalarType: int8
          # defaults to the same operator name (e.g. "_eq: _eq") if no explicit mapping is present.
          operatorMapping:
            _within: int_within
  # whether to enable _is_null
  isNull:
    # This is nested to allow for renaming of is_null here in the future
    enable: true
  graphql:
    typeName: Int_comparison_exp_with_eq_within_and_is_null
```

### Object boolean expressions

```yaml
kind: BooleanExpressionType
version: v2
definition:
  name: Album_bool_exp
  operand:
    object:
      # This is an OpenDD object type
      type: Album
      comparableFields:
        - fieldName: AlbumId
          # Use this boolean expression for this field
          booleanExpressionType: pg_Int_Comparison_exp
        - fieldName: ArtistId
          booleanExpressionType: pg_Int_Comparison_exp_with_is_null
          # This field has a complex type.
        - fieldName: Address
          booleanExpressionType: Address_bool_exp
      comparableRelationships:
        - relationshipName: artist
          # This is optional for relationships to models, and defaults to the filterExpressionType of the model
          booleanExpressionType: Artist_bool_exp
  # whether to enable _and / _or / _not
  logicalOperators:
    # This is nested to allow for renaming of logical operators here in the future
    enable: true
  # whether to enable _is_null
  isNull:
    # This is nested to allow for renaming of is_null here in the future
    enable: true
  graphql:
    typeName: App_Album_bool_exp
```

### OrderByExpression

Note: This is not an OpenDD type and cannot be used in as the type of a
field/argument.

```yaml
kind: OrderByExpression
version: v1
definition:
  # This name is unique only in the context of the `orderedType`.
  name: Album_order_by_exp
  orderedType: Album
  orderableFields:
    - fieldName: AlbumId
      enableOrderByDirections: [Asc, Desc]
    - fieldName: ArtistId
      enableOrderByDirections: [Asc]
    - # This field has an object type
      fieldName: Address
      # Use this order by expression for this object type
      orderByExpression: Address_order_by_default_exp
  # Only local relationships are permitted.
  orderableRelationships:
    - relationshipName: artist
      # orderByExpression is optional for model relationships.
      # If you don't specify it, we use the model's orderByExpression configuration.
      # For local command relationships, this is required.
      orderByExpression: Artist_order_by_default_exp
  graphql:
    expressionTypeName: App_Album_order_by_exp
```

Consequently, the changes in `kind: Model` would be:

- Bump it to `version: v2`.
- Remove `orderableFields` key.
- Remove `graphql.orderByExpressionType` key.
- Add `orderByExpression` key.

```yaml
kind: Model
version: v2
definition:
  name: Albums
  objectType: Album
  filterExpressionType: Album_bool_exp
  # This replaces orderableFields
  orderByExpression: Album_order_by_exp
  graphql:
    selectMany:
      queryRootField: App_Albums
    # There is no `orderByExpressionType` anymore within `graphql`.
```
