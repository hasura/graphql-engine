.. meta::
   :description: Deploy Hasura GraphQL engine
   :keywords: hasura, docs, deployment

Deploying Hasura GraphQL engine
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

.. note::
  This section talks in depth about deploying the Hasura GraphQL engine for a **production-like environment**.
  If you would simply like to take the Hasura GraphQL engine for a quick spin, choose from our
  :doc:`Getting started guides <../getting-started/index>`.

Deployment guides
-----------------

The Hasura GraphQL engine is a binary that is shipped as a Docker container.

Choose from the following guides to deploy the Hasura GraphQL engine and connect it to a Postgres database:

- :doc:`Deploy using Heroku <deploy/heroku>`
- :doc:`Deploy using Docker <deploy/docker>`
- :doc:`Deploy using Kubernetes <deploy/kubernetes>`

You can also check :doc:`../guides/deployment/index` for more specific examples.

Configuration
-------------

By default, Hasura GraphQL engine runs in a very permissive mode for easier development. Check out the below pages
to configure the Hasura GraphQL engine for your production environment:

- :doc:`securing-graphql-endpoint`
- :doc:`postgres-permissions`
- :doc:`GraphQL engine server configuration <graphql-engine-flags/index>`

Logs
----

For access to Hasura GraphQL engine logs, check the below page for details:

- :doc:`Logging <logging>`


.. toctree::
   :maxdepth: 1
   :titlesonly:
   :hidden:

   Deploy GraphQL engine <deploy/index>
   using-existing-heroku-database
   Server configuration <graphql-engine-flags/index>
   postgres-permissions
   Securing GraphQL endpoint <secure/index>
   Server logs <logging/index>
   Enable HTTPS <enable-https>
   allow-list
   HTTP Compression <compression>
   Production checklist <production-checklist>
   Updating GraphQL engine <update/index>
   Downgrading GraphQL engine <downgrading>
