.. meta::
   :description: Update Hasura GraphQL engine version
   :keywords: hasura, docs, deployment, update, version

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

The current latest pre-release version is:

.. raw:: html

   <code>hasura/graphql-engine:<span class="latest-prerelease-tag">prerelease</span></code>

.. note::

  Full stability with pre-release builds is not guaranteed. They are not recommended for production use.

Based on your deployment method, follow the appropriate guide to update the GraphQL engine version you're running:

- :doc:`Updating on Heroku <heroku/updating>`
- :doc:`Updating on Docker <docker/updating>`
- :doc:`Updating on Kubernetes <kubernetes/updating>`
