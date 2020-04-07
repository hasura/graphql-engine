.. meta::
   :description: Deploy Hasura GraphQL engine
   :keywords: hasura, docs, deployment

.. _deployment:

Deploying Hasura GraphQL engine
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

.. note::
  This section talks in depth about deploying the Hasura GraphQL engine for a **production-like environment**.
  If you would simply like to take the Hasura GraphQL engine for a quick spin, choose from our
  :ref:`Getting started guides <getting_started>`.

Deploy Hasura GraphQL engine
----------------------------

The Hasura GraphQL engine is a binary that is shipped as a Docker container.

Deploy Hasura from scratch
^^^^^^^^^^^^^^^^^^^^^^^^^^

Choose from the following guides to deploy the Hasura GraphQL engine and connect it to a Postgres database:

- :ref:`Deploy using Heroku <deploy_heroku>`
- :ref:`Deploy using Docker <deployment_docker>`
- :ref:`Deploy using Kubernetes <deploy_kubernetes>`

Deploy Hasura with an existing database
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

See here how to deploy the Hasura GraphQL engine with an existing database on Heroku:

- :ref:`Deploy using an existing database on Heroku <heroku_existing_db>`

Deploy Hasura with a framework
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Choose from the following frameworks to deploy Hasura:

- :ref:`Digital Ocean One-click App on Marketplace <deploy_do_marketplace>`
- :ref:`Azure Container Instances with Postgres <deploy_azure_ci_pg>`
- :ref:`Google Cloud Platform with Kubernetes engine and Cloud SQL <deploy_gc_kubernetes>`
- :ref:`Render One-Click Deploy with Managed PostgreSQL <deploy_render>`

Configuration
-------------

By default, Hasura GraphQL engine runs in a very permissive mode for easier development. Check out the below pages
to configure the Hasura GraphQL engine for your production environment:

- :ref:`GraphQL engine server configuration <hge_flags>`
- :ref:`postgres_permissions`
- :ref:`securing_graphql_endpoint`
- :ref:`Enable HTTPS <enable_https>`
- :ref:`Allow-list for queries <allow_list>`
- :ref:`Enable HTTP compression <http_compression>`

Update / downgrade Hasura GraphQL engine
----------------------------------------

To update Hasura GraphQL engine, check out the following page:

- :ref:`Update Hasura GraphQL engine <update_hge>`

To downgrade Hasura GraphQL engine, check out the following page:

- :ref:`Downgrade Hasura GraphQL engine <downgrade_hge>`

Production checklist
--------------------

For deploying Hasura GraphQL engine to production, check out our production checklist:

- :ref:`Production checklist <production_checklist>`

Logs
----

For access to Hasura GraphQL engine logs, check the below page for details:

- :ref:`Logging <hge_logs>`


.. toctree::
   :maxdepth: 1
   :titlesonly:
   :hidden:

   Deploy Hasura <deploying/index>
   Server configuration <graphql-engine-flags/index>
   postgres-permissions
   Securing GraphQL endpoint <securing/index>
   Server logs <logging/index>
   Enable HTTPS <enable-https>
   allow-list
   HTTP Compression <compression>
   Production checklist <production-checklist>
   Updating GraphQL engine <updating/index>
   Downgrading GraphQL engine <downgrading>
