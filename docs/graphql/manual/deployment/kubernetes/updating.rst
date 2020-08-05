.. meta::
   :description: Update Hasura GraphQL engine with Kubernetes deployment
   :keywords: hasura, docs, deployment, kubernetes, update

.. _kubernetes_update:

Updating Hasura GraphQL engine running on Kubernetes
====================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide will help you update the Hasura GraphQL engine running on Kubernetes. This guide assumes that you already have
the Hasura GraphQL engine running on Kubernetes.

Step 1: Check the latest release version
----------------------------------------

The current latest version is:

.. raw:: html

   <code>hasura/graphql-engine:<span class="latest-release-tag">latest</span></code>

All the versions can be found at: https://github.com/hasura/graphql-engine/releases.

Step 2: Update the container image
----------------------------------

In the ``deployment.yaml`` file, update the image tag to this latest version.

For example, if you had:

.. raw:: html

   <code>
     containers:<br>
       - image: hasura/graphql-engine:v1.0.0-alpha01
   </code>

you should change it to:

.. raw:: html

   <code>
     containers:<br>
       - image: hasura/graphql-engine:<span class="latest-release-tag">latest</span>
   </code>

Step 3: Rollout the change
--------------------------

.. code-block:: bash

  $ kubectl replace -f deployment.yaml


.. note::

  If you are downgrading to an older version of the GraphQL engine you might need to downgrade your metadata catalogue version
  as described in :ref:`downgrade_hge`



