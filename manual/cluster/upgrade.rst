Upgrading a cluster
===================

.. admonition:: Free cluster to pro-tier cluster

   Currently upgrading a free cluster to a pro-tier cluster is a manual process.
   We are working on a more seamless experience.

   For now, create a pro-tier cluster and follow :doc:`../guides/complete-cluster-migration`


Upgrading (ie: scaling) a cluster is only possible for a :doc:`pro-tier cluster <index>`.

A cluster's initial configuration is set in ``clusters.yaml``.

Subsequent changes to a cluster's configuration can be made by either modifying the
infra spec as documented :doc:`here <pro-tier/infra-spec>`, or by
using the `Pricing Calculator <https://hasura.io/pricing>`_, and then applying
these changes.


.. note::

   On ``Digital Ocean``, a cluster cannot be scaled down below the initial
   configuration of the cluster.


Using the pricing calculator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* Choose a configuration using the `Pricing Calculator
  <https://hasura.io/pricing>`_  and copy the generated ``infra code``.

* Run the command:

.. code-block:: bash

   $ hasura cluster upgrade -c <cluster-alias> --infra <infra-code>


Modifying infra specs
^^^^^^^^^^^^^^^^^^^^^
* Edit the ``clusters.yaml`` file and modify the node type and volume sizes to the new
  configuration you want. See :doc:`pro-tier/infra-spec`.

* Run the command:

.. code-block:: bash

   $ hasura cluster upgrade -c <cluster-alias>