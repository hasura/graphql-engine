.. meta::
   :description: Hasura API reference
   :keywords: hasura, docs, API, API reference

.. _api_reference:

API Reference
=============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Available APIs
--------------

+-----------------+-----------------------------------------+------------------+
| API             | Endpoint                                | Access           |
+=================+=========================================+==================+
| GraphQL         | :ref:`/v1/graphql <graphql_api>`        | Permission rules |
+-----------------+-----------------------------------------+------------------+
| Legacy GraphQL  | :ref:`/v1alpha1/graphql <graphql_api>`  | Permission rules |
+-----------------+-----------------------------------------+------------------+
| Schema/Metadata | :ref:`/v1/query <schema_metadata_api>`  | Admin only       |
+-----------------+-----------------------------------------+------------------+
| Version         | :ref:`/v1/version <version_api>`        | Public           |
+-----------------+-----------------------------------------+------------------+
| Health          | :ref:`/healthz <health_api>`            | Public           |
+-----------------+-----------------------------------------+------------------+
| PG Dump         | :ref:`/v1alpha1/pg_dump <pg_dump_api>`  | Admin only       |
+-----------------+-----------------------------------------+------------------+
| Config          | :ref:`/v1alpha1/config <config_api>`    | Admin only       |
+-----------------+-----------------------------------------+------------------+
| Explain         | :ref:`/v1/graphql/explain <explain_api>`| Admin only       |
+-----------------+-----------------------------------------+------------------+

.. _graphql_api:

GraphQL API
^^^^^^^^^^^

All GraphQL requests for queries, subscriptions and mutations are made to the GraphQL API.

See details at :ref:`api_reference_graphql`.

.. _schema_metadata_api:

Schema / metadata API
^^^^^^^^^^^^^^^^^^^^^

Hasura exposes a schema / metadata API for managing metadata for permissions/relationships or for directly
executing SQL on the underlying Postgres.

This is primarily intended to be used as an ``admin`` API to manage the Hasura schema and metadata.

See details at :ref:`metadata_apis`.

.. _version_api:

Version API
^^^^^^^^^^^

The ``/v1/version`` is a public endpoint that responds with the current server version in JSON format.

See details at :ref:`version_api_reference`.

.. _health_api:

Health check API
^^^^^^^^^^^^^^^^

The ``/healthz`` is a public endpoint that returns the server health status.

See details at :ref:`health_api_reference`.

.. _pg_dump_api:

pg_dump API
^^^^^^^^^^^

The ``/v1alpha1/pg_dump`` is an admin-only endpoint that can be used to execute
``pg_dump`` on the Postgres instance connected to Hasura. The ``pg_dump`` CLI
tool's argument can be passed as a POST request body to the API and the response
is sent back to the client.

See details at :ref:`pg_dump_api_reference`.

.. _config_api:

Config API
^^^^^^^^^^

``v1alpha1/config`` is an admin-only endpoint to get the current server
configuration.

See details at :ref:`config_api_reference`.

.. _explain_api:

Explain API
^^^^^^^^^^^

``v1/graphql/explain`` returns the Postgres plan for a query or subscription based
on the defined permissions.

See details at :ref:`explain_api_reference`.

Supported PostgreSQL types
--------------------------
You can refer to the following to know about all PostgreSQL types supported by the Hasura GraphQL engine:

- :ref:`Supported PostgreSQL types <api_postgres_types>`

.. toctree::
  :maxdepth: 1
  :hidden:

  GraphQL API <graphql-api/index>
  Schema / Metadata APIs <schema-metadata-api/index>
  Version API <version>
  Health check API <health>
  PG Dump API <pgdump>
  Config API <config>
  Explain API <explain>
  Supported PostgreSQL types <postgresql-types>
