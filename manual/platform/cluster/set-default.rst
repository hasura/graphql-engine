.. .. meta::
   :description: Deleting a Hasura cluster
   :keywords: cluster, delete

Setting a default cluster
=========================

To set a cluster as the default cluster for a project (so that you don't have to pass the ``-c`` <cluster-alias> flag with each command) or to change the current default cluster, use the ``hasura`` CLI.

.. code-block:: bash

   # Set cluster with alias <hasura> as the default
   $ hasura cluster set-default <hasura>

::

   ✓ Cluster set as default cluster-alias=hasura
