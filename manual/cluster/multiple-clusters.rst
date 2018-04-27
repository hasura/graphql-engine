Setting up multiple environments using clusters
===============================================

Multiple Hasura clusters can be added to a single Hasura project. This is a common use-case in development workflows
to simulate different environments.

For example, for a project one might have, a ``dev`` cluster for use during development, a ``staging`` cluster, which
replicates the production environment and is used to test before making the
project live, and a ``production`` cluster for the live instance of the
project.

Creating new clusters
---------------------

To create a new cluster, use the ``hasura`` CLI.

.. code-block:: bash

  $ hasura cluster create --infra free


As an example, let us create two clusters, one for staging and one for
production.

.. code-block:: bash

  $ hasura cluster create --infra free

  INFO Creating a Hasura cluster...
  INFO Hasura cluster created                        cluster=alarming52
  INFO Initializing the cluster...
  INFO Cluster initialized
  INFO Kubernetes context has been added to this system  context=alarming52

  $ hasura cluster create --infra free

  INFO Creating a Hasura cluster...
  INFO Hasura cluster created                        cluster=ambitious93
  INFO Initializing the cluster...
  INFO Cluster initialized
  INFO Kubernetes context has been added to this system  context=ambitious93


This will create two cluster under your user account. You can see them by
running:

.. code-block:: bash

  $ hasura cluster list

  Clusters available in your account:
  NO   NAME                 OWNER
  1    alarming52           you
  1    ambitious93          you



Add a cluster to a project
--------------------------

To add a cluster to a project we use:

.. code-block:: bash

  # in project directory
  $ hasura cluster add <cluster-name> -c <cluster-alias>


.. note::

   The ``-c`` flag tells to create an alias for the cluster. We can then use this alias in various other commands including git push.

Let's add the newly created two clusters to our projects.

.. code-block:: bash

  # in project directory
  $ hasura cluster add alarming52 -c staging

  ✓ SSH key (/home/wawhal/.ssh/id_rsa.pub) added to the cluster
  ✓ Cluster added to project

  $ hasura cluster add ambitious93 -c production

  ✓ SSH key (/home/wawhal/.ssh/id_rsa.pub) added to the cluster
  ✓ Cluster added to project

Now we have two clusters setup for the same project.


Deploy to a cluster
-------------------
Now whenever we make changes to our project, in the database schema, cluster
configuration or custom microservices, we just have to git-push to the correct
cluster to apply all our changes.

.. code-block:: bash

  # Make changes to the project directory and deploy to staging
  $ git push staging master

  # Finally ready to deploy to production
  $ git push production master
