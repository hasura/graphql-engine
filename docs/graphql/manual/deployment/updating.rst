.. meta::
   :description: Update Hasura GraphQL engine version
   :keywords: hasura, docs, deployment, update, version

.. _update_hge:

Updating Hasura GraphQL engine
==============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Hasura GraphQL engine runs off a Docker image and updates are as simple as changing the image tag.

The current latest version is:

.. raw:: html

   <code>hasura/graphql-engine:<span class="latest-release-tag">latest</span></code>

Based on your deployment method, follow the appropriate guide to update the GraphQL engine version you're running:

- :ref:`Updating on Heroku <heroku_update>`
- :ref:`Updating on Docker <docker_update>`
- :ref:`Updating on Kubernetes <kubernetes_update>`
