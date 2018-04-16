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

::
  
  ✓ SSH key (/home/wawhal/.ssh/id_rsa.pub) added to the cluster
  ✓ Cluster added to project

.. note::

    The ``-c`` flag tells to create an alias ``hasura`` the for the cluster. This
    name can be anything. We can then use this alias in various other commands
    including git push.
