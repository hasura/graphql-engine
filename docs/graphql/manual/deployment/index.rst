Deploying Hasura GraphQL Engine
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

.. note::
  This section talks in depth about deploying the Hasura GraphQL engine for a **production like environment**.
  If you would simply like to take the Hasura GraphQL engine for a quick spin, choose from our
  :doc:`Getting started guides <../getting-started/index>`.

Deployment guides
-----------------

The Hasura GraphQL engine is a binary that is shipped as a Docker container.

Choose from the following guides to deploy the Hasura GraphQL engine and connect it to a Postgres database:

- :doc:`Deploy using Heroku <heroku/index>`
- :doc:`Deploy using Docker <docker/index>`
- :doc:`Deploy using Kubernetes <kubernetes/index>`

You can also check :doc:`../guides/deployment/index` for more specific examples.

Configuration
-------------

By default, Hasura GraphQL engine runs in a very permissive mode for easier development. Check out the below pages
to configure Hasura GraphQL engine for your production environment:

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

   Using Heroku <heroku/index>
   Using Docker <docker/index>
   Using Kubernetes <kubernetes/index>
   Server configuration <graphql-engine-flags/index>
   Server logs <logging>
   securing-graphql-endpoint
   allow-list
   postgres-permissions
   Updating GraphQL engine <updating>
