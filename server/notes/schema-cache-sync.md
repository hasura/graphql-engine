This note is in [Hasura.Server.SchemaUpdate](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/SchemaUpdate.hs#L98).
It is referenced at:
  - line 142 of [Hasura.Server.SchemaUpdate](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/SchemaUpdate.hs#L142)
  - line 160 of [Hasura.Server.SchemaUpdate](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Server/SchemaUpdate.hs#L160)

# Schema Cache Sync


When multiple graphql-engine instances are serving on same metadata storage,
each instance should have schema cache in sync with latest metadata. Somehow
all instances should communicate each other when any request has modified metadata.

We track the metadata schema version in postgres and poll for this
value in a thread.  When the schema version has changed, the instance
will update its local metadata schema and remove any invalidated schema cache data.

The following steps take place when an API request made to update metadata:

1. After handling the request we insert the new metadata schema json
   into a postgres tablealong with a schema version.

2. On start up, before initialising schema cache, an async thread is
   invoked to continuously poll the Postgres notifications table for
   the latest metadata schema version. The schema version is pushed to
   a shared `TMVar`.

3. Before starting API server, another async thread is invoked to
   process events pushed by the listener thread via the `TMVar`. If
   the instance's schema version is not current with the freshly
   updated TMVar version then we update the local metadata.

Why we need two threads if we can capture and reload schema cache in a single thread?

If we want to implement schema sync in a single async thread we have to invoke the same
after initialising schema cache. We may loose events that published after schema cache
init and before invoking the thread. In such case, schema cache is not in sync with metadata.
So we choose two threads in which one will start listening before schema cache init and the
other after it.

What happens if listen connection to Postgres is lost?

Listener thread will keep trying to establish connection to Postgres for every one second.
Once connection established, it pushes @'SSEListenStart' event with time. We aren't sure
about any metadata modify requests made in meanwhile. So we reload schema cache unconditionally
if listen started after schema cache init start time.


