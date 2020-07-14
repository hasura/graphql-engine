.. meta::
   :description: Deploy Hasura GraphQL engine
   :keywords: hasura, docs, deployment

.. _deploying:

Deploying Hasura GraphQL engine
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------



.. note::
  This section talks in depth about deploying the Hasura GraphQL engine for a **production-like environment**.
  If you would simply like to take the Hasura GraphQL engine for a quick spin, choose from our
  :ref:`Getting started guides <getting_started>`.

Deployment guides
-----------------

The Hasura GraphQL engine is a binary that is shipped as a Docker container.

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

.. toctree::
   :maxdepth: 1
   :titlesonly:
   :hidden:

   Using Hasura Cloud <https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html>
   Using Heroku <heroku>
   Using Docker <docker>
   Using Kubernetes <kubernetes>
   Using DigitalOcean (one-click) <digital-ocean-one-click>
   Using Azure Container Instances <azure-container-instances-postgres>
   Using Google Cloud Platform & Kubernetes <google-kubernetes-engine-cloud-sql>
   Using Render (one-click) <render-one-click>
   Using Nhost (one-click) <nhost-one-click>
