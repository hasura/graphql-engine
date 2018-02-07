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

  Please **do not host** production projects on these free clusters as they don't
  have production SLAs and can go down (although unlikely).


Creating a cluster
------------------

To create a new cluster, use the ``hasura`` CLI.

.. code-block:: bash

  $ hasura cluster create --type free

  INFO Creating a Hasura cluster...
  INFO Hasura cluster created                        cluster=alarming52
  INFO Initializing the cluster...
  INFO Cluster initialized
  INFO Kubernetes context has been added to this system  context=alarming52

.. note::
  You can only create 2 free Hasura clusters.

Adding a cluster to a project
-----------------------------
To add a cluster to a project we use:

.. code-block:: bash

  $ hasura cluster add alarming52 -c hasura

  INFO Adding cluster...                             cluster-alias=hasura cluster-name=alarming52
  INFO Kubernetes context has been added to this system  context=alarming52
  INFO Cluster added to project
  INFO Setting up git remotes and pre-push hook...
  INFO remote "hasura" added: [ssh://hasura@alarming52.hasura-app.io:22/~/git/alarming52]
  INFO pre-push hook added

  $ hasura ssh-key add -c hasura


The ``-c`` flags tells to create a alias ``hasura`` the for the cluster. This
name can be anything. We can then use this alias in various other commands
including git push.

.. note::

  If you already have a cluster added with an alias, say ``hasura``, delete its entry from ``clusters.yaml`` before trying to add another cluster with the same alias.


Your clusters
-------------
To get the list of your clusters, use the ``hasura`` CLI.

.. code-block:: bash

  $ hasura clusters list

  INFO Getting clusters list...
  Clusters available In your account:
  NO   NAME         OWNER
  1    alarming52   you

  Clusters added to this project:
  NO   NAME         ALIAS
  1    alarming52   hasura


Deleting a cluster
------------------
To delete a cluster, use the ``hasura`` CLI.

.. code-block:: bash

  $ hasura cluster delete alarming52

  INFO Deleting cluster...                           cluster-name=alarming52
  INFO Cluster deleted                                                             


Advanced users
--------------
To understand the architecture of a Hasura cluster in greater detail, read this
guide on the :doc:`Hasura architecture <architecture>`.

.. toctree::
   :maxdepth: 1
   :hidden:

   architecture
   multiple-clusters

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
