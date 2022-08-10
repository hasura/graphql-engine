This note is in [Hasura.Eventing.EventTrigger](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Eventing/EventTrigger.hs#L92).
It is referenced at:
  - line 119 of [Hasura.Eventing.EventTrigger](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Eventing/EventTrigger.hs#L119)

# Maintenance mode


Maintenance mode is a mode in which users can upgrade their graphql-engine
without any down time. More on maintenance mode can be found here:
https://github.com/hasura/graphql-engine-mono/issues/431.

Basically, there are a few main things that maintenance mode boils down to:

1. No operation that may change the metadata will be allowed.
2. Migrations are not applied when the graphql-engine is started, so the
   catalog schema will be in the older version.
3. Event triggers should continue working in the new code with the older
   catalog schema i.e it should work even if there are any schema changes
   to the `hdb_catalog.event_log` table.

#1 and #2 are fairly self-explanatory. For #3, we need to support fetching
events depending upon the catalog version. So, fetch events works in the
following way now:

1. Check if maintenance mode is enabled
2. If maintenance mode is enabled then read the catalog version from the DB
   and accordingly fire the appropriate query to the events log table.
   When maintenance mode is disabled, we query the events log table according
   to the latest catalog, we do not read the catalog version for this.

