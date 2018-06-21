GraphQL API Reference
=====================

Endpoints
---------
There are two endpoints that support the  GraphQL APIs:

- All GraphQL requests for queries and mutations are ``POST`` requests to ``/v1alpha1/graphql``.

- GraphQL schema is available via a ``GET`` request to  ``/v1alpha1/graphql/schema``.

Request types
-------------
You can make the following types of requests using the GraphQL API:

.. toctree::
  :maxdepth: 1
  
  Query <query>
  Mutation <mutation>