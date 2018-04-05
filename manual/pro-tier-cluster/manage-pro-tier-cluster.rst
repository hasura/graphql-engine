.. .. meta::
   :description: How to modify cluster configuration
   :keywords: hasura, cluster, paid plans, modify, pro-tier

Managing a cluster
==================

Let's see how to interact with and modify pro-tier clusters. You need to have
one or more pro-tier clusters to perform most of the following operations.

List existing clusters
----------------------
To list all the existing clusters and see the overall status, run:

.. code-block:: bash

   $ hasura cluster list # or hasura clr ls in short


To see the status of an existing cluster
----------------------------------------
Make sure you are in the relevant project directory, and the run:

.. code-block:: bash

   $ hasura cluster status -c <cluster-alias>


Scale a cluster
---------------
A cluster's initial configuration is set in ``clusters.yaml``.

Subsequent changes to a cluster's configuration can be made by modifying the
infra spec documented in this :doc:`file <./reference-clusters-yaml>`, or by
using the `Pricing Calculator <https://hasura.io/pricing>`_, and then applying
these changes.


.. note::

   On Digital Ocean, a cluster cannot be scaled down below the initial
   configuration of the cluster.


Using the pricing calculator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* Choose a configuration using the `Pricing Calculator
  <https://hasura.io/pricing>`_.

* Click the "Install using Hasura CLI" button, and copy the generated infra code.

* Let's say the infra code from previous step is ZXPBVF. Run the command:

.. code-block:: bash

   $ hasura cluster upgrade -c <cluster-alias> --infra ZXPBVF


Modifying ``clusters.yaml`` file
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
* Edit the ``clusters.yaml`` file (see :doc:`here` for more details about
  ``clusters.yaml``), and modify the node type and volume sizes to the new
  configuration you want. Save and exit the file.

* Run the command:

.. code-block:: bash

   $ hasura cluster upgrade -c <cluster-alias>


Delete a cluster
----------------
To delete a cluster:

.. code-block:: bash

   $ hasura cluster delete <cluster-name>

Example:

.. code-block:: bash

   $ hasura cluster delete ambitious93
