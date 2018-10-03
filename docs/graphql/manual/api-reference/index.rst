API Reference
=============

GraphQL API
-----------

- All GraphQL requests for queries, subscriptions and mutations are ``POST`` requests to ``/v1alpha1/graphql``.

Request types
~~~~~~~~~~~~~

You can make the following types of requests using the GraphQL API:

- :doc:`Query/Subscription <query>`
- :doc:`Mutation <mutation>`
  

Supported PostgreSQL types
--------------------------
You can refer the following to know about all PostgreSQL types supported by the Hasura GraphQL engine:

- :doc:`Supported PostgreSQL types <postgresql-types>`

JSON APIs
---------

Hasura exposes a JSON API for querying/updating data, executing SQL on the
underlying Postgres and for managing the metadata like permissions and
relationships. Primarily intended to be used as an ``admin`` API to manage Hasura.

.. note::

   The JSON data APIs might not be in full-feature parity with
   GraphQL APIs, since features are first introduced with GraphQL and then
   ported to JSON API.
   
The endpoint is ``/v1/query`` and all requests must be ``POST``.

Here is the complete JSON API Reference:

- :doc:`Endpoints <json-api/endpoints>`
- :doc:`Query types <json-api/queries>`
- :doc:`Response structure <json-api/response>`
- :doc:`Error Codes <json-api/error-codes>`

.. toctree::
  :maxdepth: 1
  :hidden:

  Query/Subscription <query>
  Mutation <mutation>
  Supported PostgreSQL types <postgresql-types>
  JSON APIs <json-api/index>
