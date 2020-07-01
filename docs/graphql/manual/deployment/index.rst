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

.. note::
  This section talks in depth about deploying the Hasura GraphQL engine for a **production-like environment**.
  If you would simply like to take the Hasura GraphQL engine for a quick spin, choose from our
  :ref:`Getting started guides <getting_started>`.

Deployment guides
-----------------

The Hasura GraphQL engine is a binary that is shipped as a Docker container.

Choose from the following guides to deploy the Hasura GraphQL engine and connect it to a Postgres database:

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

Docker networking
-----------------

For details on how to connect to and from external APIs, check the following page:

- :ref:`networking`

Logs
----

For access to Hasura GraphQL engine logs, check the below page for details:

- :ref:`Logging <hge_logs>`


.. toctree::
   :maxdepth: 1
   :titlesonly:
   :hidden:

   Using Heroku <heroku/index>
   Using Docker <docker/index>
   Using Kubernetes <kubernetes/index>
   Server configuration <graphql-engine-flags/index>
   postgres-requirements
   securing-graphql-endpoint
   Docker networking <networking>
   Server logs <logging>
   Enable HTTPS <enable-https>
   allow-list
   HTTP Compression <compression>
   Production checklist <production-checklist>
   Custom Docker images or binaries <custom-docker-images>
   Updating GraphQL engine <updating>
   Downgrading GraphQL engine <downgrading>
