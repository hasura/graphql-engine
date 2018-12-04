Securing the GraphQL endpoint
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an access key.

Depending on your deployment method, follow one of these guides to configure an access key, and prevent public
access to your GraphQL endpoint and the Hasura console:

- :doc:`For Heroku <heroku/securing-graphql-endpoint>`
- :doc:`For Docker <docker/securing-graphql-endpoint>`
- :doc:`For Kubernetes <kubernetes/securing-graphql-endpoint>`

.. note::

  If you're looking at adding authentication and access control to your GraphQL API then head
  to :doc:`Authentication / access control <../auth/index>`.

