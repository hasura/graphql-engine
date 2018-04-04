.. .. meta::
   :description: Part 3 of a set of learning exercises meant for exploring Hasura in detail. This part takes you over data modeling & introduces the data microservice's API.
   :keywords: hasura, getting started, step 3, data modeling

=================================
Part III: Create a Hasura cluster
=================================

Now that we have a project, we need to have a Hasura cluster that becomes the target of where these projects are deployed.
As you can guess, each project can have multiple Hasura clusters as targets which would typically represent development, staging and production environments.

For now, however, let's get started with a free Hasura cluster.


Step 1: Create a free Hasura cluster
------------------------------------

The following creates a free tier Hasura cluster.

.. code-block:: bash

   $ hasura cluster create --type=free

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

Step 3: Set this as the default cluster
---------------------------------------

Because you can create multiple clusters, let's set the cluster just created as the default cluster so that you don't have
to refer to this cluster in all commands in the future using the ``-c`` flag.

.. code-block:: bash

   # Set this cluster as the default
   $ hasura cluster set-default hasura

::

   Setting default cluster...
   ✓ Cluster set as default cluster-alias=hasura


Step 4: Add your SSH key to the cluster
---------------------------------------

To execute ``git push`` commands to deploy to the cluster, you need to put your SSH public key on the cluster so that
your cluster's git remote can identify you securely.

.. code-block:: bash

   # Add an SSH key to the default cluster
   $ hasura ssh-key add

::

   Adding SSH key from <dir-path>/.ssh/id_rsa.pub ...
   Waiting for configureation to be synced...
   ✓ SSH key (<dir-path>/.ssh/id_rsa.pub) added cluster=hasura [test42]

If you don't have an SSH key already, an SSH key will get created for you in your ``~/.ssh`` folder.

Step 5: Deploy the project to the cluster
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
