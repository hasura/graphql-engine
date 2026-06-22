# Security regression: `exists` filter targeting a role-hidden field (AISLE-2026-0087)

Role `user_1` may select the `Album` model but `ArtistId` is hidden from it
(`TypePermissions` grant only `["AlbumId", "Title"]`). The request's filter uses
an `exists` predicate whose **target field is the hidden `ArtistId`**.

Expected: rejected with `PermissionError::ObjectFieldNotFound` from the
`get_field` check added to `to_exists_expression` (`crates/plan/src/filter.rs`).
`exists` is dispatched separately from ordinary comparisons, so it needs (and
now has) its own field-permission check; without it the exists target was
resolved straight from `type_mappings`, bypassing the role's read permissions.

This path is only reachable from user-supplied filter input; admin-defined
permission predicates compile through `process_model_predicate` and never reach
this function.
