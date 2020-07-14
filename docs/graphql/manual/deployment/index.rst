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
- :ref:`Deploy using Digital Ocean One-click App on Marketplace <deploy_do_marketplace>`
- :ref:`Deploy using Azure Container Instances with Postgres <deploy_azure_ci_pg>`
- :ref:`Deploy using Google Cloud Platform with Kubernetes engine and Cloud SQL <deploy_gc_kubernetes>`
- :ref:`Deploy using Render One-click Deploy with Managed PostgreSQL <deploy_render>`
- :ref:`Deploy using Nhost One-click Deploy with Managed PostgreSQL, Storage, and Auth <deploy_nhost>`
- `Deploy using Instant GraphQL on AWS RDS (blog) <https://hasura.io/blog/instant-graphql-on-aws-rds-1edfb85b5985>`__

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
   Securing GraphQL endpoint <securing-graphql-endpoint>
   Server logs <logging>
   Enable HTTPS <enable-https>
   allow-list
   HTTP Compression <compression>
   Production checklist <production-checklist>
   Custom Docker images or binaries <custom-docker-images>
   Updating GraphQL engine <updating-graphql-engine>
   Downgrading GraphQL engine <downgrading>
