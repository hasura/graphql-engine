.. meta::
   :description: Notes on remote schemas with Hasura
   :keywords: hasura, docs, remote schema, notes

.. _schema_points:

Points to remember
------------------

Remote schema fields nomenclature
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Top-level field names need to be unique across all merged schemas (*case-sensitive match*).
- Types with the *exact same name and structure* will be merged. But types with the *same name but different
  structure* will result in type conflicts.


Schema refreshing
^^^^^^^^^^^^^^^^^

For versions <= ``v1.0.0-beta.2``, GraphQL schema of each added remote server is refreshed every time a
metadata modifying operation like adding tables/functions, defining relationships/permissions etc. is done.

From ``v1.0.0-beta.3`` onwards, a remote server's GraphQL schema is cached and refreshed only when user
explicitly reloads remote schema by clicking the ``Reload`` button on the console or
by making a :ref:`reload_remote_schema<api_remote_schemas>` metadata API request


Current limitations
^^^^^^^^^^^^^^^^^^^

- Nodes from different GraphQL servers cannot be used in the same query/mutation. All top-level fields have to be
  from the same GraphQL server.
- Subscriptions on remote GraphQL servers are not supported.

These limitations will be addressed in upcoming versions.

Extending the auto-generated GraphQL schema fields
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For some use cases, you may need to extend the GraphQL schema fields exposed by the Hasura GraphQL engine
(*and not merely augment as we have done above*) with a custom schema/server. To support them, you can use
community tooling to write your own client-facing GraphQL gateway that interacts with the GraphQL engine.

.. note::

  **Adding an additional layer on top of the Hasura GraphQL engine significantly impacts the performance provided by
  it out of the box** (*by as much as 4x*). If you need any help with remodelling these kinds of use cases to use the
  built-in remote schemas feature, please get in touch with us on `Discord <https://discord.gg/vBPpJkS>`__.
