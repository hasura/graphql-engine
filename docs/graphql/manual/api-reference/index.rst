API Reference
=============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

GraphQL API
-----------

All GraphQL requests for queries, subscriptions and mutations are made to the GraphQL API.

All requests are ``POST`` requests to the ``/v1/graphql`` and ``/v1alpha1/graphql`` endpoint.

.. note::

   ``/v1/graphql`` endpoint returns HTTP 200 status codes for all responses.
   This is a **breaking** change from ``/v1alpha1/graphql`` behaviour, where
   request errors and internal errors were responded with 4xx and 5xx status
   codes.

Request types
^^^^^^^^^^^^^

The following types of requests can be made using the GraphQL API:

- :doc:`Query / Subscription <query>`
- :doc:`Mutation <mutation>`

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
  Supported PostgreSQL types <postgresql-types>
