GraphQL API Reference
=====================

Endpoints
---------

- All GraphQL requests for queries, subscriptions and mutations are ``POST`` requests to ``/v1alpha1/graphql``.

Request types
-------------
You can make the following types of requests using the GraphQL API:

- :doc:`Query/Subscription <query>`
- :doc:`Mutation <mutation>`
  

Supported PostgreSQL types
--------------------------
You can refer the following to know about all PostgreSQL types supported by the Hasura GraphQL engine:

- :doc:`Supported PostgreSQL types <postgresql-types>`

.. toctree::
  :maxdepth: 1
  :hidden:

  Query/Subscription <query>
  Mutation <mutation>
  Supported PostgreSQL types <postgresql-types>
