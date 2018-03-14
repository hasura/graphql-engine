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
installed on it.  This Kubernetes cluster is usually runs on a cloud
infrastructure like Google Cloud, Azure or AWS.

All Hasura clusters are free for development purposes or for hobby projects.

For hosting production projects, please get in touch with us at
support@hasura.io.

.. note::

  Please **do not host production projects** on these free clusters as they don't
  have production SLAs and can go down (although unlikely).

See:
^^^^

.. toctree::
   :maxdepth: 1

   create
   add
   list
   delete
   multiple-clusters
   set-default

..
  uncomment this when we have danava
  Free clusters
  -------------
  One can get a free cluster
  There are free clusters that a user can create.
  Free clusters are free forever, but they have limited resources.
  Paid clusters
  -------------
  There are paid clusters as well. In a paid cluster, the user has a choice of
  the cluster (CPU/RAM/Disk Size etc.) and region they want to install on.
