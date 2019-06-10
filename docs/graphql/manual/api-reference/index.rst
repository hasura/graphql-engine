API Reference
=============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Available APIs
--------------

+-----------------+----------------------------------------+------------------+
| API             | Endpoint                               | Access           |
+=================+========================================+==================+
| GraphQL         | :ref:`/v1/graphql <graphql_api>`       | Permission rules |
+-----------------+----------------------------------------+------------------+
| Legacy GraphQL  | :ref:`/v1alpha1/graphql <graphql_api>` | Permission rules |
+-----------------+----------------------------------------+------------------+
| Schema/Metadata | :ref:`/v1/query <schema_metadata_api>` | Admin only       |
+-----------------+----------------------------------------+------------------+
| Version         | :ref:`/v1/version <version_api>`       | Public           |
+-----------------+----------------------------------------+------------------+
| Health          | :ref:`/healthz <health_api>`           | Public           |
+-----------------+----------------------------------------+------------------+
| PG Dump         | :ref:`/v1alpha1/pg_dump <pg_dump_api>` | Admin only       |
+-----------------+----------------------------------------+------------------+
| Config          | :ref:`/v1alpha1/config <config_api>`   | Admin only       |
+-----------------+----------------------------------------+------------------+

.. _graphql_api:

GraphQL API
^^^^^^^^^^^

All GraphQL requests for queries, subscriptions and mutations are made to the GraphQL API.

See details at :doc:`graphql-api/index`

.. _schema_metadata_api:

Schema / Metadata API
^^^^^^^^^^^^^^^^^^^^^

Hasura exposes a Schema / Metadata API for managing metadata for permissions/relationships or for directly
executing SQL on the underlying Postgres.

This is primarily intended to be used as an ``admin`` API to manage Hasura schema and metadata.

See details at :doc:`schema-metadata-api/index`

.. _version_api:

Version API
^^^^^^^^^^^

A ``GET`` request to the public ``/v1/version`` endpoint responds with the current server version
in JSON format:

.. code-block:: js

   {"version": "v1.0.0-alpha01"}

.. _health_api:

Health check API
^^^^^^^^^^^^^^^^

A ``GET`` request to the public ``/healthz`` endpoint will respond with ``200``
if GraphQL Engine is ready to serve requests and there are no inconsistencies
with the metadata. The response will be ``500`` if there are metadata
inconsistencies and you should use the console or check the server logs to find
out what the errors are.


.. _pg_dump_api:

pg_dump API
^^^^^^^^^^^

The ``/v1alpha1/pg_dump`` is an admin-only endpoint that can be used to execute
``pg_dump`` on the Postgres instance connected to Hasura. The ``pg_dump`` CLI
tool's argument can be passed as POST request body to the API and the response
is sent back to the client.

See details at :doc:`pgdump`.

.. _config_api:

Config API
^^^^^^^^^^

``v1alpha1/config`` is an admin-only endpoint to get the current server
configuration.

See details at :doc:`config`.

Supported PostgreSQL types
--------------------------
You can refer to the following to know about all PostgreSQL types supported by the Hasura GraphQL engine:

- :doc:`Supported PostgreSQL types <postgresql-types>`

.. toctree::
  :maxdepth: 1
  :hidden:

  GraphQL API <graphql-api/index>
  Schema / Metadata APIs <schema-metadata-api/index>
  PG Dump API <pgdump>
  Config API <config>
  Supported PostgreSQL types <postgresql-types>
