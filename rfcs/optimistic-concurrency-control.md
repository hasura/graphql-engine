## Motivation

Currently, console is not aware of metadata changes that happen outside of
console (say another user working on the same project). This causes problems
when a user tries to modify something that has already been changed on
the server - the operation could result in an error or could cause unintended
side effects. For example,

(Say two users are working on the same server through different console pages)

1. user-1 wants to untrack a table 't' but before the user hit the untrack
   table button, user-2 untracks the table 't'. This results in user-1 seeing
   an error such as "table 't' not found".

1. user-1 wants to untrack a table 't' but before the user hit the untrack table
   button, user-2 adds a relationship from table 'x' to table 't'.  This
   results in user-1's request for untrack table going through but also drops
   the relationship created on table 'x' by user-2.

For a better user experience, we want the console to be aware of the changes to
metadata that happen outside of it so that the users can be warned when they
are working on stale metadata.

## Approach

Optimistic concurrency control is generally used to avoid such issues resulting
from concurrent updates to a shared resource. The idea is that the server
maintains an internal `resourceVersion` for a resource which is bumped on every
change to the resource. To modify a resource, a client will send the
`resourceVersion` of the resource it is trying to modify along with the
operation. The server will only let the operation go through if the resource
`resourceVersion` the client sends is the same as what is on the server. This
ensures that the resource has not been modified since the client has last seen
it.

Let's consider example 1 from and above and look at the request flow in the
presence of `resourceVersion`. Let's say the metadata is at version `v` and
both the users open their consoles and they receive that the metadata's
`resourceVersion` as `v`.

1. user-2 sends a request to untrack table `t` with `resourceVersion` set to
   `v`.
1. The server sees that the `resourceVersion` of the metadata currently is `v`,
   so it lets the request goes through and the `resourceVersion` is now bumped
   to `w` and is sent back in the response.
1. user-1, unaware of the untrack operation, sends the same untrack table 't'
   with `resourceVerison` set to `v`.
1. The server sees that the `resourceVersion` of the metadata (`w`) is more
   recent than what the client has sent (`v`), so it rejects the request and
   responds with the new `resourceVersion`.
1. user-1's console informs the user that the metadata has been modified on
   the server and can prompt to refetch the metadata from the server.

## Changes required

1. We need a mechanism to pass the `resourceVersion` to all our metadata
   operations.  Currently they are POSTed to `v1/metadata`, with the payload as
   follows:

   ```
   {
      "type": "operation_name",
      "version": version_of_the_operation,
      "args": OperationArgs
   }
   ```

   We have two options here:

   1. we can either extend the json payload with an additional key

      ```
      {
         "type": "operation_name",
         "version": version_of_the_operation,
         "args": OperationArgs,
         "resourceVersion": resourceVersion
      }
      ```

   1. `resourceVersion` can be a url parameter as follows:

      ```
      POST /v1/metadata?resourceVersion=x
      {
         "type": "operation_name",
         "version": version_of_the_operation,
         "args": OperationArgs
      }
      ```

   Both seem reasonable so we can go with whatever is easier to implement. Note
   that some of these operations might want to ignore `resourceVersion`, for
   example `export_metadata` which doesn't modify the metadata.

1. The response of `export_metadata` will now change as follows:

   ```
   {
      "resourceVersion": x,
      "metadata": Metadata
   }
   ```

   So we'll need a `v2` version of `export_metadata` API.

1. The response of all metadata modifying operations will now need to include
   the new `resourceVerison` post modification.

## Subtleties

1. `resourceVersion` will need to be bumped on `reload_metadata` call as:

   1. `reload_metadata` can cause the metadata to become inconsistent, so a query
      such as `drop_inconsistent_metadata` would mean different things before and
      after the `reload_metadata` call. For example, user working on console 1 sees
      that there are 'x' inconsistent objects and is about to click
      `drop_inconsistent_metadata` but before that happened, user working on
      console-2 reloaded the metadata and now there are 'y' inconsistent objects.
      `drop_inconsistent_metadata` issued by user on console-1 would go through as
      the `resourceVersion` on the server wouldn't have changed as the metadata is
      not modified on the database.

   1. `reload_metadata` can change the semantics of some operations, such as adding
      remote schema permissions. Let's say the user-1 is defining remote schema
      permissions but user-2 reloaded the metadata which fetched the changed
      schema of a remote schema, should the 'add remote schema permissions' call
      go through? It shouldn't because the permission is defined assuming an
      earlier schema.

   1. Further, we've been thinking of using `resourceVersion` as a way to sync
      schema across multiple instances instead of the current schema sync
      mechanism based on listen/notify. This wouldn't work if `resourceVersion` is
      not bumped when `reload_metadata` is called.

1. `resourceVersion` will need to be bumped when `run_sql` on any source
   results in any metadata cascades, say a table is renamed.
