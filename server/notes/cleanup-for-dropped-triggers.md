This note is in [Hasura.RQL.DDL.Metadata](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Metadata.hs#L95).
It is referenced at:
  - line 201 of [Hasura.RQL.DDL.Metadata](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Metadata.hs#L201)
  - line 144 of [Hasura.RQL.Types.Eventing.Backend](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/Types/Eventing/Backend.hs#L144)

# Cleanup for dropped triggers

There was an issue (https://github.com/hasura/graphql-engine/issues/5461)
fixed (via https://github.com/hasura/graphql-engine/pull/6137) related to
event triggers while replacing metadata in the catalog prior to metadata
separation. The metadata separation solves the issue naturally, since the
'hdb_catalog.event_triggers' table is no more in use and new/updated event
triggers are processed in building schema cache. But we need to drop the
database trigger and archive events for dropped event triggers. This is handled
explicitly in @'runReplaceMetadata' function.

