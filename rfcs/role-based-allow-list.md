## Metadata examples

1. Global allowlist
```
allowlist:
- collection: bar
  scope:          #optional
    global: true  #required
```

2. Role-based allowlist
```
allowlist:
- collection: bar
  scope:               #required for role-based allowlist
    global: false      #required
    roles: [foo, bar]  #required for role-based allowlist
```

## Metadata format

1. The allowlist is an array of allowlist entries. 
2. Each allowlist entry is an object, of one of 2 forms: global and role-based.

    i. Each allowlist entry is required to contain a `collection`.

    ii. A role-based allowlist entry is required to have a `scope`, containing `global: false` and `roles` (which is a non-empty array of roles).

    iii. A global allowlist entry is not required to have a `scope` field, but if it does, we expect `global: true` and no roles. (This means that if `scope` is not provided, then it is assumed to be a global collection, which ensures backwards compatibility.)

3. Collection names must be unique in the entire allowlist (i.e. there must not be multiple allowlist entries with the same collection name).
4. Incremental API `add_collection_to_allowlist` takes an additional (optional) field `scope`.
5. We introduce a new incremental API `update_scope_of_collection_in_allowlist`. This has the fields `collection` and `scope` (both required).

## API Behaviour

1. Global collections are available for ALL roles.

2. Role-based collections are _additionally_ available to the configured roles.

3. When `add_collection_to_allowlist` does not have a `scope`:

    a) If the collection already exists (global or role-based), no-op success with a response saying "collection already exists and given scope is ignored. To change scope, use the `update_scope_of_collection_in_allowlist` API". The main reason for not erroring here is because currently `add_collection_to_allowlist` is idempotent and we want to be backwards compatible.

    b) If the collection doesn't exist, then it is created as a global collection.

4. When `add_collection_to_allowlist` has a `scope` field:

    a) If the collection already exists (global or role-based) in allowlist, no-op success with a response saying "collection already exists and given scope is ignored. To change scope, use the `update_scope_of_collection_in_allowlist` API".

    b) If the collection doesn't exist in allowlist, then it is created with the given `scope`. If `scope` doesn't conform to the format as specified in "Metdata format" section, then an appropriate error is thrown. E.g. "roles is missing for collection with non-global scope", "roles cannot be empty for collection with non-global scope", "roles should not be provided for collection with global scope".

5. For `drop_collection_from_allowlist`:

    a) If the collection exists (global or role-based) in allowlist, then we drop the collection.

    b) If the collection doesn't exist in allowlist, then an error is thrown saying collection does not exist in allowlist.

6. For `update_scope_of_collection_in_allowlist`:

    a) If the collection exists (global or role-based) in allowlist, then we set its scope to the provided `scope`. If `scope` doesn't conform to the format specified in "Metdata format" section, then an appropriate error is thrown. E.g. "roles is missing for collection with non-global scope", "roles cannot be empty for collection with non-global scope", "roles should not be provided for collection with global scope".

    b) If the collection doesn't exist in allowlist, then an error is thrown saying collection does not exist in allowlist.

**Backwards compatibility NOTE**

When a metadata spec configured with role-based allowlists is used with older Hasura version, then `scope` key is silently ignored and the collection becomes global. We should document this. Ideally, even with allowlist, role-based authz will kick-in for the collections.
