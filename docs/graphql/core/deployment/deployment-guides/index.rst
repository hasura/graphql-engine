.. meta::
   :description: Deploy Hasura GraphQL engine
   :keywords: hasura, docs, deployment

.. _deployment_guides:

Deployment guides
=================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The Hasura GraphQL engine is a binary that is shipped as a Docker container.

Choose from the below guides to deploy the Hasura GraphQL engine and connect it to a Postgres database.

.. _one_click_deployment_guides:

One-click deployment options
----------------------------

If you want to take Hasura for a spin and check out the features, the following one-click deployments will be useful for you:

- :ref:`Deploy using Hasura Cloud <cloud_getting_started>` (**recommended**)
- :ref:`Deploy using Heroku <heroku_one_click>`
- :ref:`Deploy using Render One-click Deploy with Managed PostgreSQL <deploy_render>`
- :ref:`Deploy using Nhost One-click Deploy with Managed PostgreSQL, Storage, and Auth <deploy_nhost>`

.. _all_deployment_guides:

Deployment guides
-----------------

Choose from the full list of deployment guides:

- :ref:`Deploy using Hasura Cloud <cloud_getting_started>` (**recommended**)
- :ref:`Deploy using Docker <deployment_docker>`
- :ref:`Deploy using Kubernetes <deploy_kubernetes>`
- :ref:`Deploy using Heroku <deploy_heroku>`
- :ref:`Deploy using Digital Ocean One-click App on Marketplace <deploy_do_marketplace>`
- :ref:`Deploy using Azure Container Instances with Postgres <deploy_azure_ci_pg>`
- :ref:`Deploy using Google Cloud Platform with Kubernetes engine and Cloud SQL <deploy_gc_kubernetes>`
- `Deploy using Instant GraphQL on AWS RDS (blog) <https://hasura.io/blog/instant-graphql-on-aws-rds-1edfb85b5985>`__
- :ref:`Deploy using Render One-click Deploy with Managed PostgreSQL <deploy_render>`
- :ref:`Deploy using Nhost One-click Deploy with Managed PostgreSQL, Storage, and Auth <deploy_nhost>`

.. admonition:: Custom Docker images or binaries

  If you need a custom Docker image or binary for GraphQL engine, please see :ref:`this page <custom_docker_image>`.

.. toctree::
   :maxdepth: 1
   :titlesonly:
   :hidden:

   Using Hasura Cloud <https://hasura.io/docs/1.0/graphql/cloud/getting-started/index.html>
   Using Docker <docker>
   Using Kubernetes <kubernetes>
   Using Heroku (one-click) <heroku>
   Using DigitalOcean (one-click) <digital-ocean-one-click>
   Using Azure Container Instances <azure-container-instances-postgres>
   Using Google Cloud Platform & Kubernetes <google-kubernetes-engine-cloud-sql>
   Using Render (one-click) <render-one-click>
   Using Nhost (one-click) <nhost-one-click>
