Part II: Create a Hasura cluster
================================

Now that we have a project, we need to have a Hasura cluster that will become the target of where this project is deployed.
As you can guess, each project can have multiple Hasura clusters as targets. This can be used to create different environments
like development, staging, production, etc.

Let's get started with a free Hasura cluster.

Step 1: Create a free Hasura cluster
------------------------------------

The following creates a free tier Hasura cluster.

.. code-block:: bash

   $ hasura cluster create --infra free

::
   
   Creating a Hasura cluster...
   • Kubernetes context has been added to this system context=test42
   ✓ Hasura cluster created cluster-name=test42
     # Add this cluster to your project with an alias 'hasura':
     $ hasura cluster add test42 -c hasura

Step 2: Add this cluster to your project
----------------------------------------

Take a note of your cluster name, from the example above it is ``test42``.

Run the following from inside your project directory:

.. code-block:: bash

   # Add the cluster and alias it to an easier name
   $ hasura cluster add test42 -c hasura

::
   
   Adding cluster...
   ✓ Cluster added to project

The command above, adds the cluster to your project and aliases it to ``hasura`` so that we don't
have to remember the cluster name.

Now, in all subsequent commands we can refer to this cluster with the name ``hasura``. You could have named this to ``dev``, or
``staging`` or ``qa`` or anything of your preference.

Step 3: Deploy the project to the cluster
-----------------------------------------

To deploy the project to the cluster, run the following:

.. code-block:: bash

   # Commit the project files and git push to deploy
   $ git add . && git commit -m "Initial commit"
   $ git push hasura master   # hasura is the cluster alias to deploy to


The ``git push`` will deploy everything, ie: the project conf, migrations and microservices, to the cluster.


Next: Explore the Hasura cluster
--------------------------------

Next, head to :doc:`explore-hasura-cluster`.
