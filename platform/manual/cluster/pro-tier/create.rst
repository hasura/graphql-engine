Creating a pro-tier cluster
===========================

This section takes you over the process of creating a cluster on Hasura's pro-tier and linking it to a project.

.. note::

   Having an active :doc:`billing account <../../billing/index>` is mandatory to create pro-tier clusters.


Follow the below steps to create a pro-tier cluster and link it to a project:

Choosing cluster configuration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can generate your cluster's configuration using the `Pricing Calculator
<https://hasura.io/pricing>`_.

This will generate an ``infra code`` (eg: ZXPBVF) which can then be used to create a cluster
of the chosen configuration.

Creating the cluster
^^^^^^^^^^^^^^^^^^^^
After you get the ``infra code`` from the above step, run the following hasura CLI command:

.. code-block:: bash

   $ hasura cluster create --infra <infra-code>

This will create a cluster with the configuration described by the ``infra code`` value.

.. note::

   If you create the cluster while in a project directory, the above
   will add the created cluster to the project and also add the cluster's infra specs to the ``clusters.yaml`` file
   so that the infra requirements of the project can be saved and version controlled. See :doc:`infra-spec` for more details.

   The added infra spec will have an alias assigned to it. By default, the alias is ``hasura``.
   An infra spec with alias <some-cluster-alias> defines the config of the cluster which is running with  alias <some-cluster-alias>

   To give your own alias to the infra spec, run this instead of the above:

   .. code-block:: bash

      $ hasura cluster create --infra <infra-code> --add-as <cluster-alias>

   Now ``<cluster-alias>`` will be set as the alias for the infra spec and the cluster created.


Adding the cluster to a Hasura project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
If you executed, ``cluster create`` inside a project directory, the cluster will already be added to the project.

Otherwise, see :doc:`this <../add>` to add the created cluster to a project. This will also add the infra spec to the project
as described in the note above.

Example
^^^^^^^
Let's assume the infra code generated from the Pricing Calculator is ``ZXPBVF``,
and assume we want alias of this cluster to be ``prod``.

Then the flow to create a cluster and adding it to a project is one of the follows:

.. code-block:: bash

   # from inside project directory
   $ hasura cluster create --infra ZXPBVF --add-as prod

OR

.. code-block:: bash

   # from outside project directory
   $ hasura cluster create --infra ZXPBVF
   # say cluster name is: ambitious93
   $ cd <project-directory>
   $ hasura cluster add ambitious93 -c prod

Advanced
^^^^^^^^

You can also write declarative configuration of your cluster. The file
``clusters.yaml`` contains
cluster configuration. Check out :doc:`infra-spec` and some :doc:`sample infra specs <sample-infra-specs>`.

Once you have added your cluster infra specs in ``clusters.yaml``, run the
following command to create a cluster.

.. code-block:: bash

   $ hasura cluster create --cluster <cluster-alias>


where ``<cluster-alias>`` is the cluster alias in the ``clusters.yaml``.

After this, add the cluster to the project by following :doc:`this <../add>`.
