Custom GraphQL resolvers
========================

Custom GraphQL resolvers can be used to add extra internal fields to the top-level fields exposed in the Hasura
GraphQL engine schema.

Custom resolvers can be added to the Hasura GraphQL schema using the `Apollo's graphql-tools library <Apollo_>`__.
The extra fields can be resolved by either using custom code or by delegating to another GraphQL API.

.. note::

  Adding an additional layer on top of Hasura GraphQL engine significantly impacts the performance provided by it out
  of the box. Hence, if you don't need to make changes to the fields exposed by the Hasura GraphQL engine, see
  :doc:`schema-stitching` for a more performant solution

**Example**:

*TODO: add custom resolver examples with custom code (?) and external API delegation (weather API)*

.. _Apollo: https://github.com/apollographql/graphql-tools