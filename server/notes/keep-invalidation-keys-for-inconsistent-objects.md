This note is in [Hasura.RQL.DDL.Schema.Cache](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Schema/Cache.hs#L1261).
It is referenced at:
  - line 207 of [Hasura.RQL.DDL.Schema.Cache](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Schema/Cache.hs#L207)

# Keep invalidation keys for inconsistent objects

After building the schema cache, we prune InvalidationKeys for objects
that no longer exist in the schema to avoid leaking memory for objects
that have been dropped. However, note that we *don’t* want to drop
keys for objects that are simply inconsistent!

Why? The object is still in the metadata, so next time we reload it,
we’ll reprocess that object. We want to reuse the cache if its
definition hasn’t changed, but if we dropped the invalidation key, it
will incorrectly be reprocessed (since the invalidation key changed
from present to absent).
