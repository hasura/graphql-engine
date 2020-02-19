.. meta::
   :description: Secure the Hasura GraphQL endpoint
   :keywords: hasura, docs, deployment, secure

.. _securing-graphql-endpoint:

Securing the GraphQL endpoint
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an admin secret key.

Depending on your deployment method, follow one of these guides to configure an admin secret key, and prevent public
access to your GraphQL endpoint and the Hasura console:

- :doc:`For Heroku <heroku>`
- :doc:`For Docker <docker>`
- :doc:`For Kubernetes <kubernetes>`

.. note::

  If you're looking at adding access control rules for your data to your GraphQL API then head
  to :doc:`Authentication / access control <../auth/index>`.

.. toctree::
   :maxdepth: 2

   Heroku <heroku>
   Docker <docker>
   Kubernetes <kubernetes>


