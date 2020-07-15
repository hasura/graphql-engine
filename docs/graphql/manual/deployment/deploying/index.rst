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

The Hasura GraphQL engine is a binary that is shipped as a Docker container.

Choose from the below guides to deploy the Hasura GraphQL engine and connect it to a Postgres database.

One-Click deployment guides
---------------------------

If you want to take Hasura for a spin and check out the features, the following one-click deployments will be useful for you:

- `Deploy using Hasura Cloud <https://hasura.io/docs/cloud/1.0/manual/getting-started/index.html>`__ (**recommended**)
- :ref:`Deploy using Heroku <heroku_one_click>`
- :ref:`Deploy using Render One-click Deploy with Managed PostgreSQL <deploy_render>`
- :ref:`Deploy using Nhost One-click Deploy with Managed PostgreSQL, Storage, and Auth <deploy_nhost>`

Production deployment guides
----------------------------

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
