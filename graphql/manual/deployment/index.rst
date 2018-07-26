Deploying Hasura GraphQL Engine
===============================

.. note::
  This section talks in depth about deploying the Hasura GraphQL engine for a **production like environment**.
  If you would simply like to take the Hasura GraphQL engine for a quick spin, choose from our
  :doc:`Getting started guides <../getting-started/index>`.

The Hasura GraphQL engine is a binary that is shipped as a Docker container. Choose from the following guides to
deploy the Hasura GraphQL engine and connect it to a Postgres database.

- :doc:`Deploy using Heroku <heroku/index>`
- :doc:`Deploy using Docker <docker/index>`

By default, Hasura GraphQL engine runs in a very permissive mode for easier development. Check out the below pages
to configure Hasura GraphQL engine for your production environment.

- :doc:`securing-graphql-endpoint`
- :doc:`postgres-permissions`
- :doc:`GraphQL engine server configuration <graphql-engine-flags/index>`


.. toctree::
   :maxdepth: 1
   :titlesonly:
   :hidden:

   Using Heroku <heroku/index>
   Using Docker <docker/index>
   securing-graphql-endpoint
   postgres-permissions
   GraphQL engine server configuration <graphql-engine-flags/index>
   Updating GraphQL engine <updating>
