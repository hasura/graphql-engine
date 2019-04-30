GraphQL API Reference
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

All GraphQL requests for queries, subscriptions and mutations are made to the GraphQL API.

Endpoint
--------

All requests are ``POST`` requests to ``/v1/graphql`` (or ``/v1alpha1/graphql``) endpoint.

.. note::

   ``/v1/graphql`` endpoint returns HTTP 200 status codes for all responses.
   This is a **breaking** change from ``/v1alpha1/graphql`` behaviour, where
   request errors and internal errors were responded with 4xx and 5xx status
   codes.

Request types
-------------

The following types of requests can be made using the GraphQL API:

- :doc:`Query / Subscription <query>`
- :doc:`Mutation <mutation>`

.. toctree::
  :maxdepth: 1
  :hidden:

  Query / Subscription <query>
  Mutation <mutation>
