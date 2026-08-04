## Nested updates

### Related issues

- https://github.com/hasura/graphql-engine/issues/669
- https://github.com/hasura/graphql-engine/issues/1573
- https://github.com/hasura/graphql-engine/issues/3366
- https://github.com/hasura/graphql-engine/issues/2952

### Use case

During object insert and update mutations, it should be possible to perform the 
following actions for nested objects:

- insert/upsert (create new object and relate)
- update (update object and ensure relation)
- delete (unrelate and delete object)
- link (relate with an existing object)
- unlink (unrelate an existing object)
- replace (unlink existing and link new)

### Proposed API

**Nested Inserts:**

No changes

**Nested updates:**

For example, for the schema:
- `author (id, name, address_id)`
- `article (id, title, author_id)` 
- `address (id, street_address, city)`.
```
update_authors(
    _set, 
    _inc,
    where,
    _relationships: {
        # array relationship
        articles: {
            _insert: { data, on_conflict}
            _update: {_set, where, _relationships}
            _delete: BoolExp
            _link: BoolExp
            _unlink: BoolExp
        }
        # object relationship
        address: {
            _insert: { data, on_conflict} 
            _update: {_set, _relationships}  # Note that this update has no "where"
            _delete: true
            _link: SingleBoolExp  # Special kind of "where" that should return 1 result else error
            _unlink: true
        }
    }
)
```

***How it would work:***

Relationships between objects are inferred via reference keys _(foreign-keys or manual)_. 

In the above example, for the `articles` array relationship the reference key is `article.author_id -> author.id` and for the `address` object relationship the reference key is `author.address_id -> address.id`.

- Insert/Upsert: Create new nested oject and set the reference key to establish the relationship
- Update: Update the nested object and update the reference key if required
- Delete: Delete the nested object and set the reference key to `null` if required (if nullable)
- Link: Set the reference key to establish the relationship
- Unlink: Set the reference key to `null` to remove the relationship (if nullable)
- Replace: Can be achieved via `_unlink`/`_delete` + `_link`/`_insert` combination
