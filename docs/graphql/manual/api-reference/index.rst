API Reference
=============

+----------------------------------------+-----------------+------------------+
| Endpoint                               | API             | Type             |
+========================================+=================+==================+
| :ref:`/v1alpha1/graphql <graphql_api>` | GraphQL         | Permission rules |
+----------------------------------------+-----------------+------------------+
| :ref:`/v1/query <schema_metadata_api>` | Schema/Metadata | Admin only       |
+----------------------------------------+-----------------+------------------+
| :ref:`/v1/version <version_api>`       | Version         | Public           |
+----------------------------------------+-----------------+------------------+
| :ref:`/healthz <health_api>`           | Health          | Public           |
+----------------------------------------+-----------------+------------------+
| :ref:`/v1alpha1/pg_dump <pg_dump_api>` | PG Dump         | Admin only       |
+----------------------------------------+-----------------+------------------+

.. _graphql_api:

GraphQL API
-----------

All GraphQL requests for queries, subscriptions and mutations are made to the GraphQL API.

All requests are ``POST`` requests to the ``/v1alpha1/graphql`` endpoint.

Request types
^^^^^^^^^^^^^

The following types of requests can be made using the GraphQL API:

- :doc:`Query / Subscription <query>`
- :doc:`Mutation <mutation>`

.. _schema_metadata_api:

Schema / Metadata API
---------------------

Hasura exposes a Schema / Metadata API for managing metadata for permissions/relationships or for directly
executing SQL on the underlying Postgres.

This is primarily intended to be used as an ``admin`` API to manage Hasura schema and metadata.
   
All requests are ``POST`` requests to the ``/v1/query`` endpoint.

Request types
^^^^^^^^^^^^^

The following lists all the types of requests that can be made using the Schema/Metadata API:

- :ref:`Schema / Metadata API query types <Query>`

.. _version_api:

Version API
-----------

A ``GET`` request to this endpoint responds with the current server version in JSON format:

.. code-block:: js

   {"version": "v1.0.0-alpha44"}

.. _health_api:

Health check API
----------------

``GET`` request to this public endpoint (``/healthz``) will respond with ``200``
if GraphQL Engine is ready to serve requests and there are no inconsistencies
with the metadata. The response will be ``500`` if there are metadata
inconsistencies and you should use the Console or check the server logs to find
out what the errors are.


.. _pg_dump_api:

pg_dump API
-----------

This is an admin-only API that can be used to execute ``pg_dump`` on the
Postgres instance connected to Hasura. The ``pg_dump`` CLI tool's argument can
be passed as POST request body to the API and the response is sent back to the
client.

Find more details at :doc:`pgdump`.

Supported PostgreSQL types
--------------------------
You can refer to the following to know about all PostgreSQL types supported by the Hasura GraphQL engine:

- :doc:`Supported PostgreSQL types <postgresql-types>`

.. toctree::
  :maxdepth: 1
  :hidden:

  Query / Subscription <query>
  Mutation <mutation>
  Schema / Metadata APIs <schema-metadata-api/index>
  PG Dump API <pgdump>
  Supported PostgreSQL types <postgresql-types>
