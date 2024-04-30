# Boolean Expression Types in OpenDD

## Motivation

All mutations in NDC are implemented as standalone procedures, whose semantics
are opaque to OpenDD. In order to provide the ability to filter rows on which a
procedure would apply, NDC is
[introducing](https://github.com/hasura/ndc-spec/pull/83) boolean expression
types as procedure arguments. Consequently, we would also need to support
boolean expressions as arguments in OpenDD. Since OpenDD already uses boolean
expressions (called filter expressions) for filtering models as a part of select
many, we would want to unify the two definitions.

## Proposal

### kind: ObjectFilterExpressionType

The first proposal is to introduce a new OpenDD object type for filter
expressions. This will allow us to exact;u define filter expressions and reuse
them across models and command arguments.

```yaml
kind: ObjectFilterExpressionType
version: v1
definition:
  name: AuthorFilterExpression
  objectType: Author
  dataConnectorName: my_db
  dataConnectorObjectType: author
  filterableFields:
    - fieldName: id
      comparisonExpression:
        operators:
            enableAll: true
    - fieldName: name
      comparisonExpression:
        operators:
          enableSpecific: ["_eq", "_like"]
    graphql:
      typeName: Author_bool_exp
```

### Move type mappings to kind: ObjectType

Currently type mappings are specified as a part of model / command source, where
we define how the object types used in a particular model map to object types
used in the source data connector. It was designed this way so that
`kind: ObjectType` was defined purely in terms of OpenDD and coupling to a
connector was done only in `kind: Model` / `kind: Command`. However, now that
mutations will be opaque commands, we expect an ObjectType used within a model
to be commonly reused across multiple commands, making it verbose to redefine
the type mappings for every command.

Hence, the second proposal is to move type mappings as a part of
`kind: ObjectType` and removed from `kind: Model` / `kind: Command`. This is
also more intuitive to understand for a reader of OpenDD.

```yaml
kind: ObjectType
version: v1
definition:
  name: Author
  fields:
    - name: id
      type: Int!
    - name: name
      type: String
  dataConnectorTypeMapping: # Optional key
    - dataConnectorName: my_db
      dataConnectorObjectType: author
      fieldMapping:
        id:
          column: author_id
        name:
          column: author_name
```

### Update kind: Model / kind: Command

Since filter expressions and type mappings are now being defined elsewhere,
updates are needed to `kind: Model`.

- Remove the `typeMappings` key from the `source` field of the model definition.
- Remove the `filterExpressionType` key from the `graphql` field of the model
  definition.
- Remove the `filterable_fields` key from the model definition.
- Introduce a `filterExpressionType` key to the model definition, which is
  optional and points to an existing `ObjectFilterExpressionType` that is
  defined on the same object type as the model's object type.

```yaml
kind: Model
version: v1
definition:
  name: Authors
  objectType: Author
  filterExpressionType: AuthorFilterExpression
  orderableFields: ...
  graphql:
    orderByExpressionType: Author_order_by
    selectMany:
      queryRootField: authors
  source:
    dataConnectorName: author
```

Similarly, we would remove `typeMappings` key from the `source` field of the
command definition.

## Alternatives Considered

A couple of alternatives were considered, but rejected.

### Implicit boolean expression types, fully qualified on reference

```yaml
kind: Command
version: v1
definition:
  arguments:
    - name: where
      type:
        booleanExpression:
          objectType: Author
          filterableFields: ...
          graphql:
            typeName: Author_bool_exp
```

There would be too much repition and it would be the inconsistent with the rest
of OpenDD where you can only refer to explicitly defined types.

### Boolean expression types attached to kind: ObjectType

```yaml
kind: ObjectType
version: v1
definition:
  name: Author
  fields: ...
  filterExpressions:
    name: AuthorFilterExpression
    dataConnectorName: my_db
    dataConnectorObjectType: author
    filterableFields: ...
    graphql:
      typeName: Author_bool_exp
```

The problem with this is it is unclear whether the name `AuthorFilterExpression`
shares the same namespace with other OpenDD types.

- If yes, it is inconsistent to define a named OpenDD type that is not its own
  kind.
- If no, then type references become more complicated:

```yaml
- name: id
  type:
    regular: Int!
- name: where
  type:
    booleanExpression: AuthorFilterExpression
```
