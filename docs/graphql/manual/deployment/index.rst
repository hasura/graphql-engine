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

Choose from the below guides to deploy the Hasura GraphQL engine and connect it to a Postgres database.

One-Click deployment guides
^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you want to take Hasura for a spin and check out the features, the following one-click deployments will be useful for you:

- `Deploy using Hasura Cloud <https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html>`__ (**recommended**)
- :ref:`Deploy using Heroku <heroku_one_click>`
- :ref:`Deploy using Render One-click Deploy with Managed PostgreSQL <deploy_render>`
- :ref:`Deploy using Nhost One-click Deploy with Managed PostgreSQL, Storage, and Auth <deploy_nhost>`

Production deployment guides
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you're looking to use Hasura in a production-like environment, choose from the following guides:

- `Deploy using Hasura Cloud <https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html>`__ (**recommended**)
- :ref:`Deploy using Heroku <deploy_heroku>`
- :ref:`Deploy using Docker <deployment_docker>`
- :ref:`Deploy using Kubernetes <deploy_kubernetes>`
- :ref:`Deploy using Digital Ocean One-click App on Marketplace <deploy_do_marketplace>`
- :ref:`Deploy using Azure Container Instances with Postgres <deploy_azure_ci_pg>`
- :ref:`Deploy using Google Cloud Platform with Kubernetes engine and Cloud SQL <deploy_gc_kubernetes>`
- `Deploy using Instant GraphQL on AWS RDS (blog) <https://hasura.io/blog/instant-graphql-on-aws-rds-1edfb85b5985>`__

.. admonition:: Custom Docker images or binaries

  If you need a custom Docker image or binary for GraphQL engine, please see :ref:`this page <custom_docker_image>`


Configuration
-------------

By default, Hasura GraphQL engine runs in a very permissive mode for easier development. Check out the below pages
to configure the Hasura GraphQL engine for your production environment:

- :ref:`GraphQL engine server configuration <hge_flags>`
- :ref:`Postgres requirements <postgres_permissions>`
- :ref:`Securing the GraphQL endpoint <securing_graphql_endpoint>`
- :ref:`Enable HTTPS <enable_https>`
- :ref:`Allow-list of operations <allow_list>`
- :ref:`HTTP compression <http_compression>`
- :ref:`Updating GraphQL engine <update_hge>`
- :ref:`Downgrading GraphQL engine <downgrade_hge>`


Logs
----

For access to Hasura GraphQL engine logs, check the below page for details:

- :ref:`Logging <hge_logs>`

Production checklist
--------------------

If you're moving your Hasura GraphQL engine to production, consult the following guide:

- :ref:`Production checklist <production_checklist>`

Custom Docker images or binaries
--------------------------------

If you need a custom Docker image or binary, check out the following guide:

- :ref:`Custom Docker images or binaries <custom_docker_image>`

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
