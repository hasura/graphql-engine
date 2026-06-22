# Security regression: filter by a role-hidden field (AISLE-2026-0087)

Role `user_1` may select the `Album` model but its `TypePermissions` grant only
`["AlbumId", "Title"]` — **`ArtistId` is hidden** (see
`tests/static/metadata.json`). The request selects a visible field but
**filters** by the hidden `ArtistId`.

Expected: rejected with `PermissionError::ObjectFieldNotFound` ("no permission
to select from field ArtistId in type Album") from the `get_field` check in
`to_field_comparison_expression` (`crates/plan/src/filter.rs`). Without that
check the predicate would compile (or fail with a misleading internal error),
letting a caller infer hidden values through predicate side-channels.
