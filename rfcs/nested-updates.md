## Nested updates

### Related issues

- https://github.com/hasura/graphql-engine/issues/1573
- https://github.com/hasura/graphql-engine/issues/3366
- https://github.com/hasura/graphql-engine/issues/2952

### Use case

During object insert and update mutations, it should be possible to perform the 
following actions for nested objects:

- insert/upsert
- update
- delete
- replace
- link
- unlink

### Proposed API

**Nested Inserts:**

No changes

**Nested updates:**

For example, for schema `author (id, name, address_id)`, `article (id, title, author_id)` 
and `address (id, street_address, city)`.
```
update_authors(
    _set, _inc,
    where,
    _relationships: {
        # array relationship
        articles: {
            _link: BoolExp
            _unlink: BoolExp
            _insert: { data, on_conflict}
            _update: {_set, where, _relationships}
            _delete: BoolExp
        }
        # object relationship
        address: {
            # Special kind of "where" that should return 1 result.
            # If more results then error
            _link: SingleBoolExp
            _unlink: true/false
            _insert: { data, on_conflict}
            _update: {_set, _relationships} # Note that this update has no where
            _delete: true/false
        }
    }
)
```