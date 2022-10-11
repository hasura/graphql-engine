# Remote Schema Customization

## Motivation

- Prevent name conflicts between remote schemas and other sources
- Allow customization of third-party schemas to better fit with local naming conventions

## Spec

Add an optional `customization` object to the `add_remote_schema` API with the following form:

```yaml
customization:
  # if root_fields_namespace is absent, the fields
  # are merged into the query root directly
  root_fields_namespace: "something"
  type_names:
    prefix: some_prefix
    suffix: some_suffix
    # mapping takes precedence over prefix and suffix
    mapping:
      old_name: new_name
  field_names:
    - parent_type: old_type_name
      prefix: some_prefix
      suffix: some_suffix
      # mapping takes precedence over prefix and suffix
      mapping:
        old_name: new_name
```

- Type name prefix and suffix will be applied to all types in the schema except the root types (for query, mutation and subscription), types starting with `__`, standard scalar types (`Int`, `Float`, `String`, `Boolean`, and `ID`), and types with an explicit mapping.
- Root types, types starting with `__`  and standard scalar types may only be customized with an explicit mapping.
- Fields that are part of an interface must be renamed consistently across all object types that implement that interface.

## Implementation approach

- After obtaining the remote schema via introspection we build customization functions (and their inverses) for the types and fields in the schema.
- Customizations are validated against the schema to ensure that:
  - Field renamings of objects and interfaces are consistent
  - Customization does not result in two types, or two fields of the same type, being renamed to the same name.
- The remote schema is customized using the customization functions to rename types and fields.
- The field parser generators are modified so that they recognise the customized schema, but generate GraphQL queries with the original names to be sent to the remote server
- We can uses aliases on the fields to make sure the response object has the customized fields names.
