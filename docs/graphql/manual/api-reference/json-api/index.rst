JSON API Reference
==================

The JSON API provides the following features:

1. CRUD APIs on PostgreSQL tables with a MongoDB-esque JSON query syntax. (respects permissions set on tables)
2. Execute SQL on the underlying Postgres database, supports schema modifying actions. (``admin`` only)
3. Modify Hasura metadata (permissions rules and relationships) (``admin`` only)


**See:**

.. toctree::
  :maxdepth: 1

  Endpoints <endpoints>
  Query types <queries>
  Response structure <response>
  Error Codes <error-codes>
