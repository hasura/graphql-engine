Part I: Create a Hasura project and cluster
===========================================

A Hasura project is a directory (ie: a code repo) that contains all the configuration files, the database
migrations, along with the source code and configuration of your custom microservices. This project directory should be
a git repo, so that you can ``git push hasura master`` to deploy everything in the repo to a Hasura cluster.


Create a 'hello-world' project
------------------------------

Run the following command:

.. code-block:: bash

   $ hasura clone hasura/hello-world

::

   Cloning project...
   ✓ Project cloned directory=<dir-path>/hello-world


This will 'clone' the `hasura/hello-world <https://hasura.io/hub/projects/hasura/hello-world>`_ project from
`hasura.io/hub <https://hasura.io/hub>`_ into your current directory.

.. admonition:: Note

   ``hasura/hello-world`` is a starter project that contains a few database migrations to add a sample schema and
   some sample data to let you start experimenting quickly.

   You can clone any project from the `hub <https://hasura.io/hub>`_ and use that as a starting point for your new project.

Understand the project structure
--------------------------------
A Hasura project has a particular directory structure and it has to be maintained strictly, else ``hasura CLI`` will not work
as expected.

Move to the project directory we just cloned.

Run the following command:

.. code-block:: bash

   $ cd hello-world


Every Hasura project follows the below structure:

.. code-block:: bash

   .
   ├── .hasura
   ├── hasura.yaml
   ├── clusters.yaml
   ├── conf
   │   ├── authorized-keys.yaml
   │   ├── auth.yaml
   │   ├── ci.yaml
   │   ├── domains.yaml
   │   ├── filestore.yaml
   │   ├── gateway.yaml
   │   ├── http-directives.conf
   │   ├── notify.yaml
   │   ├── postgres.yaml
   │   ├── routes.yaml
   │   └── session-store.yaml
   ├── migrations
   │   ├── <1504788327_create_table_user.down.yaml>
   │   ├── <1504788327_create_table_user.down.sql>
   │   ├── <1504788327_create_table_user.up.yaml>
   │   └── <1504788327_create_table_user.up.sql>
   └── microservices
       ├── <adminer>
       │   └── k8s.yaml
       └── <flask>
           ├── src/
           ├── k8s.yaml
           └── Dockerfile

.. note::

   In our ``hello-world`` project, the ``microservices`` directories will by empty right now

Read more about :doc:`../project/index`

Create a Hasura cluster
-----------------------

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

Now, in all subsequent commands we can refer to this cluster with the alias ``hasura``. You could have aliased this to ``dev``,
or ``staging`` or anything else according to your preference.

Step 3: Deploy the project to the cluster
-----------------------------------------

To deploy the project to the cluster, run the following:

.. code-block:: bash

   # Commit the project files and git push to deploy
   $ git add . && git commit -m "Initial commit"
   $ git push hasura master   # hasura is the cluster alias to deploy to


The ``git push`` will deploy everything, ie: the project conf, migrations and microservices, to the cluster.

The hasura cluster comes with a bunch of in-built microservices for Database, Authentication, Files, Routing etc. The GraphQL API is served by the Data Microservice.

Read more about :doc:`../cluster/index`


Next: GraphQL Schema
--------------------

Next, let's head to :doc:`graphql-schema`.
