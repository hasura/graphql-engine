.. meta::
   :description: Deploy Hasura GraphQL engine
   :keywords: hasura, docs, deployment

.. _deployment:

Deploying Hasura GraphQL engine
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Deployment guides
-----------------

Choose from the following guides to deploy the Hasura GraphQL engine and connect it to a Postgres database:

- `Deploy using Hasura Cloud <https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html>`__
- :ref:`Deploy using Heroku <deploy_heroku>`
- :ref:`Deploy using Docker <deployment_docker>`
- :ref:`Deploy using Kubernetes <deploy_kubernetes>`

You can also check :ref:`guides_deployment` for more specific examples.

.. admonition:: Custom Docker images or binaries

  If you need a custom Docker image or binary for GraphQL engine, please see :ref:`this page <custom_docker_image>`


Configuration
-------------

By default, Hasura GraphQL engine runs in a very permissive mode for easier development. Check out the below pages
to configure the Hasura GraphQL engine for your production environment:

- :ref:`securing_graphql_endpoint`
- :ref:`postgres_permissions`
- :ref:`GraphQL engine server configuration <hge_flags>`

Logs
----

For access to Hasura GraphQL engine logs, check the below page for details:

- :ref:`Logging <hge_logs>`


.. toctree::
   :maxdepth: 1
   :titlesonly:
   :hidden:

   Deploying Hasura GraphQL engine <deploying/index>
   Server configuration <graphql-engine-flags/index>
   postgres-requirements
   Securing GraphQL endpoint <securing-graphql-endpoint/index>
   Server logs <logging/index>
   Enable HTTPS <enable-https>
   allow-list
   HTTP Compression <compression>
   Production checklist <production-checklist>
   Custom Docker images or binaries <custom-docker-images>
   Updating GraphQL engine <updating/index>
   Downgrading GraphQL engine <downgrading>
   Deployment guides <guides/index>
