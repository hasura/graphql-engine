Deploying Hasura GraphQL Engine
===============================

.. note::
  This section talks about deloying the Hasura GraphQL engine to a **production environment**. If you would like to
  quickly take the Hasura GraphQL engine for a spin, check out our
  :doc:`Getting started guide <../getting-started/index>`.

The Hasura GraphQL engine is a binary that is shipped as a Docker container. Choose from the following guides to
deploy the Hasura GraphQL engine and connect it to a Postgres database.

- :doc:`Deploy using Heroku <heroku/index>`
- :doc:`Deploy using Docker <docker/index>`

By default, Hasura GraphQL engine runs in a very permissive mode for easier development. Check out the below docs to
configure Hasura GraphQL engine for your production environment.

- :doc:`postgres-credentials`
- :doc:`GraphQL engine server flags <graphql-engine-flags/index>`


.. toctree::
   :maxdepth: 1
   :titlesonly:
   :hidden:

   Using Heroku <heroku/index>
   Using Docker <docker/index>
   postgres-credentials
   graphql-engine-flags/index
