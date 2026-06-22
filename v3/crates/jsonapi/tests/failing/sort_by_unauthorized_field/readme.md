# Security regression: JSON:API sort accepts unauthorized fields (AISLE-2026-0087)

This test reproduces a missing-allowlist authorization gap (CWE-285) in the
JSON:API `sort` parameter.

## Setup

- Role `user_1` may select the `Album` model, but the `Album` `TypePermissions`
  grant `user_1` only `["AlbumId", "Title"]` — **`ArtistId` is hidden** for this
  role (`admin` can see all three fields). See `tests/static/metadata.json`.
- The request selects only the visible fields (`fields[Album]=AlbumId,Title`)
  but sorts by the hidden field (`sort=-ArtistId,AlbumId`).

## Expected (secure) behaviour

The request should be **rejected** — `sort` must be allowlisted against the
role-visible field set, exactly as `validate_sparse_fields` already rejects
hidden fields in `fields[...]`.

## The fix

The gap was that `crates/plan/src/order_by.rs` (`resolve_field_operand`)
resolved the order-by column from the underlying object type's `type_mappings`
without consulting the role-filtered `OutputObjectTypeView`. It now calls
`object_type.get_field(..)` first, which rejects any field the role cannot
select with a user-facing `PermissionError::ObjectFieldNotFound`.

Because this is the single order-by planning path shared by all frontends, the
fix covers JSON:API, GraphQL and SQL uniformly. The snapshot in this directory
therefore now captures the secure behaviour: an authorization error rather than
a leaking `Ok(DocumentData { .. })`.
