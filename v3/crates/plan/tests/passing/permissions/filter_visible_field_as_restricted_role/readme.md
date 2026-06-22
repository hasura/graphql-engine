# Positive case: a restricted role may still filter by a VISIBLE field (AISLE-2026-0087)

Runs as `user_1`, who can see `["AlbumId", "Title"]` on `Album` (but not
`ArtistId`). Filtering and selecting by the visible `Title` must succeed —
proving the field-level read-permission checks in `crates/plan/src/filter.rs`
constrain only hidden fields, not all predicates for a restricted role.
