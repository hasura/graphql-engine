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

Deploy the Hasura GraphQL engine and connect it to a Postgres database:

- :ref:`One-click deployment guides <one_click_deployment_guides>`
- :ref:`Production deployment guides <production_deployment_guides>`

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
