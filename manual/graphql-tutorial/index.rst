.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _graphql_complete_tutorial:

GraphQL on Hasura tutorial: Learn by example
============================================

This is a simple, in-depth tutorial to understand how Hasura works and how you
can use GraphQL APIs with Hasura.

.. note::
    The Hasura CLI is needed for working with Hasura. If you haven't installed it yet,
    please see :doc:`../install-hasura-cli`

At the end of this tutorial, you will understand:

- what Hasura projects and clusters are,
- how to use the Hasura GraphQL APIs,
- how to make use of postgres views to avoid writing custom resolvers,
- how to write custom resolvers and deploy on Hasura,
- how to implement schema stitching with Hasura and third-party APIs.

This tutorial is split across the following sections:

.. toctree::
  :maxdepth: 1

  get-started
  graphql-schema
  graphql-apis
  relationships
  access-control
  customise-schema-views
  write-your-own-resolvers
