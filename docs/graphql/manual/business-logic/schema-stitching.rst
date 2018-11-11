Hasura GraphQL schema stitching
===============================

Schema stitching is the process of creating a single GraphQL schema from multiple underlying GraphQL APIs.

Typically, schema stitching is done using `Apollo's graphql-tools library <Apollo_>`__. But adding an additional layer on
top of Hasura GraphQL engine significantly impacts the performance provided by it out of the box. To avoid this
performance hit, Hasura has implemented its own schema stitching feature which you can access via the console.

If you don't have an external GraphQL API and instead need to write custom code to create the new top-level fields, use
`Apollo's graphql-tools library <Apollo_>`__ to create a custom GraphQL endpoint first and then stitch it with the Hasura
GraphQL endpoint.

.. note::

  Using Hasura's schema stitching feature you cannot modify the original Hasura GraphQL engine schema fields. It simply
  adds the top-level fields of the external GraphQL endpoints to the Hasura GraphQL schema.

  If you need to make changes to the fields exposed by the Hasura GraphQL engine, see :doc:`custom-resolvers`

**Example**:

*TODO: add schema stitching with Hasura example*


.. _Apollo: https://github.com/apollographql/graphql-tools
