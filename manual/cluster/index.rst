.. .. meta::
   :description: What is a Hasura cluster?
   :keywords: hasura, CLI, cluster


.. _hasuractl-manual:

.. highlight:: bash

.. _hasura_cluster_doc:

Hasura cluster
==============

..
   - What is a hasura cluster?: k8s cluster, runs on some infra, free vs paid
   - Kubernetes architecture of the hasura platform: the hasura platform in k8s terms
   - Configuration: Declarative configuration under the user's control that the controller applies
   - Control loop of the controller
   - Provisioning


A Hasura cluster is a `Kubernetes <https://kubernetes.io>`_ cluster, with
Hasura backend APIs and some internal microservices (like Postgres, Nginx etc.)
installed on it.  This Kubernetes cluster is usually running on a cloud
infrastructure like Google Cloud, Azure or AWS.

Hasura provides free clusters as well as paid :doc:`pro-tier clusters <pro-tier/index>`.

Hasura free-tier clusters are for development purposes or for hobby projects.

For hosting production projects, Hasura pro-tier clusters should be used.

.. note::

  Do not host production projects on free clusters as they don't
  have production SLAs and can go down (although unlikely).

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
   connect-kubectl
   pro-tier/index