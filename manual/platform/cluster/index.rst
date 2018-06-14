.. .. meta::
   :description: What is a Hasura cluster?
   :keywords: hasura, CLI, cluster


.. _hasuractl-manual:

.. highlight:: bash

.. _hasura_cluster_doc:

Hasura cluster
==============

A Hasura cluster is basically a `Kubernetes <https://kubernetes.io>`_ cluster underneath, which is usually running on a
cloud infrastructure like ``Digital Ocean``, ``Google Cloud``, ``Azure`` or ``AWS``.

Hasura clusters are used to deploy :doc:`Hasura projects <../project/index>` and host its constituent
:doc:`microservices <../microservices/index>` (ie: a Postgres database, Hasura backend APIs, custom microservices, etc).

Hasura provides free clusters as well as paid pro-tier clusters.
Hasura free-tier clusters are for development purposes or for hobby projects. For hosting production projects,
Hasura :doc:`pro-tier clusters <pro-tier/index>` should be used.

.. note::

  Do not host production projects on free clusters as they don't
  have production SLAs and stability.

See:
^^^^

.. toctree::
   :maxdepth: 1

   create
   add
   list
   delete
   upgrade
   multiple-clusters
   set-default
   reusing-a-cluster
   connect-kubectl
   pro-tier/index
