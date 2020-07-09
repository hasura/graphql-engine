.. meta::
   :description: Update Hasura GraphQL engine version
   :keywords: hasura, docs, deployment, update, version

.. _update_hge:

Updating Hasura GraphQL engine
==============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Update guides
-------------

The Hasura GraphQL engine runs off a Docker image and updates are as simple as changing the image tag.

Based on your deployment method, follow the appropriate guide to update the GraphQL engine version you're running:

- :ref:`Updating on Heroku <heroku_update>`
- :ref:`Updating on Docker <docker_update>`
- :ref:`Updating on Kubernetes <kubernetes_update>`

Latest available versions
-------------------------

Stable version
**************

The current latest stable version is:

.. raw:: html

   <code>hasura/graphql-engine:<span class="latest-release-tag">latest</span></code>

Pre-release version
*******************

The current latest pre-release version is:

.. raw:: html

   <code>hasura/graphql-engine:<span class="latest-prerelease-tag">prerelease</span></code>

.. note::

  Full stability with pre-release builds is not guaranteed. They are not recommended for production use.

.. toctree::
   :titlesonly:
   :hidden:

   Heroku <heroku>
   Docker <docker>
   Kubernetes <kubernetes>
