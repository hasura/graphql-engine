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

.. code-block:: console

   $ hasura cluster create --type=free

::
   
   INFO Creating a Hasura cluster...
   INFO Hasura cluster created                        cluster=depute95
   INFO Initializing the cluster...
   INFO Cluster initialized
   INFO Add this cluster to your project:
    > hasura cluster add test42 -c test42

Run the ``cluster create`` command and you will see output similar to what you see above.

Step 2: Add this cluster to your project
----------------------------------------

Take a note of your cluster name, in the example above ``test42``.

.. code-block:: bash

   # Add the cluster and alias it to an easier name
   $ hasura cluster add test42 -c hasura

::
   
   INFO Adding cluster...                             cluster-alias=hasura cluster-name=test42
   INFO Cluster added to project
   INFO Setting up git remotes and pre-push hook...
   INFO remote "hasura" added: [ssh://hasura@test42.hasura-app.io:22/~/git/test42]
   INFO pre-push hook added

The command above, adds the cluster to your project and renames it (or aliases it) to ``hasura`` so that we don't
have to remember the cluster name.

Now, in all subsequent commands we can refer to this cluster with the name ``hasura``. You could've named this to ``dev``, or
``staging`` or ``qa`` or anything of your preference.


Step 3: Set this as the default cluster
---------------------------------------

Because you can create multiple clusters, let's set the cluster just created to the default cluster so that you don't have
to refer to this cluster in the future.

.. code-block:: bash

   # Set this cluster as the default
   $ hasura cluster set-default hasura

::
   
   INFO Setting default cluster...                    cluster-alias=hasura
   INFO Cluster set as default


Step 4: Add your SSH key to the cluster
---------------------------------------

To execute `git push` commands to deploy to the cluster, you need to put your SSH public key on the cluster so that
your cluster's git remote can identify you securely.

.. code-block:: bash

   # Add an SSH key to the default cluster
   $ hasura ssh-key add

::

   INFO Adding SSH key from /Users/suppandi/.ssh/id_rsa.pub ...
   INFO SSH key added to cluster

If you don't have an SSH key already, an SSH key will get created for you in your ``~/.ssh`` folder.

Next: Explore the Hasura cluster
--------------------------------

Next, head to :doc:`Part IV: Explore the Hasura cluster <4-explore-hasura-cluster>`.
