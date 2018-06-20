.. .. meta::
   :description: Creating a Hasura cluster
   :keywords: cluster, create

Creating a cluster
==================

.. admonition:: Pro-tier cluster

    For creating a **pro-tier cluster**, refer :doc:`pro-tier/create`

To create a new **free Hasura cluster**, use the ``hasura`` CLI:

.. code-block:: bash

  $ hasura cluster create --infra free
  
  • A new free-tier Hasura cluster will be created
  Continue? (y/n) y
  • kubectl context has been added to this system context=absolve54
  • Hasura cluster created cluster-name=absolve54
    # Add this cluster to your project with an alias 'hasura':
    $ hasura cluster add absolve54 -c hasura

.. note::
    You can only create 2 free Hasura clusters.
