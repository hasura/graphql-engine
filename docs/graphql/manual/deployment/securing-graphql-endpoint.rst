.. meta::
   :description: Secure the Hasura GraphQL endpoint
   :keywords: hasura, docs, deployment, secure

.. _securing_graphql_endpoint:

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

- :ref:`For Heroku <heroku_secure>`
- :ref:`For Docker <docker_secure>`
- :ref:`For Kubernetes <kubernetes_secure>`

.. admonition:: Limitations

  There is no direct length limit for the admin secret apart from the HTTP header size set by the webserver/gateway which defaults to 5MB on Hasura.

.. note::

  If you're looking at adding access control rules for your data to your GraphQL API then head
  to :ref:`Authentication / access control <auth>`.

