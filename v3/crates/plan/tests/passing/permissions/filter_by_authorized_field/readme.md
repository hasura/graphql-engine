# Positive case: a role that CAN see the field may filter by it (AISLE-2026-0087)

No `session_variables.json`, so this runs as `admin`, who can see all of
`Album`'s fields (`["AlbumId", "ArtistId", "Title"]`). Filtering and selecting
by `ArtistId` must succeed — proving the field-level read-permission checks
added in `crates/plan/src/filter.rs` reject only fields hidden from the
_current_ role and do not over-block legitimate access.
