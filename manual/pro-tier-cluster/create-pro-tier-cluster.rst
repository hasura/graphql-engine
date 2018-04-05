Creating a pro-tier cluster
=============================

This section takes your over the process of creating a cluster on Hasura's Pro-tier.

.. note::

   Make sure you have an active billing account to create Pro-tier clusters.
   See :doc:`here <./manage-billing>` to enable billing.


Creating a cluster
------------------

Choose cluster configuration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can generate your cluster's configuration using the `Pricing Calculator
<https://hasura.io/pricing>`_.

Create the cluster
^^^^^^^^^^^^^^^^^^
After the above step, follow the ``hasura`` CLI commands to create a cluster of
that configuration.

It is usually of the form:

.. code-block:: bash

   $ hasura cluster create --infra ZXPBVF

Following the above will create a cluster, add it to the current Hasura project,
and add its configuration to the ``clusters.yaml`` file, with a default the
cluster alias ``hasura``.

To add your own alias, run:

.. code-block:: bash

   $ hasura cluster create --infra ZXPBVF --add-as prod

``prod`` will be set as the alias for the created cluster.


Example
-------
Let's assume the infra code generated from the Pricing Calculator is ``ZXPBVF``,
and assume we want alias of this cluster to be ``prod`` (you can leave out
``--add-as`` to add as default ``hasura`` alias).

Then the flow to create a cluster and push a project to this:

.. code-block:: bash

   $ hasura cluster create --infra ZXPBVF --add-as prod
   # git push the project to this cluster
   $ git push prod master


Advanced
--------
You can also write declarative configuration of your cluster. The file
``clusters.yaml`` (at the top-level of a Hasura project directory) can contain
cluster configuration. Check out the :doc:`reference documentation for
clusters.yaml <./reference-clusters-yaml>` and some :doc:`sample cluster
configurations <./sample-cluster-configs>`.

Once you have added your cluster configuration in ``clusters.yaml``, run the
following command to create a cluster.

.. code-block:: bash

   $ hasura cluster create --cluster <cluster-alias>


where ``<cluster-alias>`` is the cluster alias in the ``clusters.yaml``.

After this, you should add the cluster to the project. For reference, see
:doc:`this <../hasuractl/hasura_cluster_add>`.
