.. .. meta::
   :description: Describing the hasura project directory structure
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl, hasuracli

.. _hasura-deploy-project:

.. highlight:: bash

Deploying a project
===================

A Hasura project can be deployed on a :doc:`../cluster/index` by simply running ``git push hasura master``

Step 1: Get a Hasura cluster
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

See :doc:`../cluster/create`

Step 2: Add cluster to project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
See :doc:`../cluster/add`

Step 3: Deploy project to cluster
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Commit all the files in the project folder and run ``git push hasura master`` (where *hasura* is the cluster alias)

This will deploy everything, including your custom microservices, database migrations and project configuration to the cluster.