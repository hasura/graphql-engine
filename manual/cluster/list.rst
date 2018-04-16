.. .. meta::
   :description: Listing created hasura clusters
   :keywords: cluster, list

Listing your clusters
=====================

To get the list of all your created clusters and those added to the current project, use the ``hasura`` CLI.

.. code-block:: bash

  $ hasura clusters list

  You have created 2 of 2 free clusters.
    # delete clusters that are not required using:
    $ hasura cluster delete [cluster-name]

  Clusters available in your account:
  NOS     NAME             VERSION     TYPE            STATUS     OWNER
  1       conversion36     0.15.31     hasura-free     READY      you
  2       colliery88       0.15.31     hasura-free     READY      you

  Clusters added to this project:
  NOS   NAME         ALIAS
  1     colliery88   hasura
