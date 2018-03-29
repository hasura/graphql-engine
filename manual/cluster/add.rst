.. .. meta::
   :description: Adding a Hasura cluster to a project
   :keywords: cluster, add

Adding a cluster to a project
=============================

To add a cluster to a project we use:

.. code-block:: bash

  # in project directory
  # to add cluster named 'alarming52' as alias 'hasura' to project
  $ hasura cluster add alarming52 -c hasura

  INFO Adding cluster...                             cluster-alias=hasura cluster-name=alarming52
  INFO Kubernetes context has been added to this system  context=alarming52
  INFO Cluster added to project
  INFO Setting up git remotes and pre-push hook...
  INFO remote "hasura" added: [ssh://hasura@alarming52.hasura-app.io:22/~/git/alarming52]
  INFO pre-push hook added

After adding the cluster to the project, run the follwing to add your ssh key to the cluster to enable ``git push``

.. code-block:: bash

  $ hasura ssh-key add -c hasura

.. note::

    The ``-c`` flag tells to create an alias ``hasura`` the for the cluster. This
    name can be anything. We can then use this alias in various other commands
    including git push.


.. note::

  If you already have an existing cluster added with an alias, say ``hasura``, delete its entry from ``clusters.yaml`` before trying to add another cluster with the same alias.
