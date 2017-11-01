.. meta::
   :description: Part 2 of a set of learning exercises meant for exploring Hasura in detail. This part introduces the Auth service's User & Session management model.
   :keywords: hasura, getting started, step 2

================================
Part II: Create a Hasura project
================================

A Hasura project is a folder (representing a code repo) that contains all the configuration files, the database migrations and the source code and cofiguration for your custom microservices. This project folder should be a git repo, so that you can `git push hasura master` to deploy everything in the repo to the cluster.


Step 1: Create a 'base' project
-------------------------------

Run the following command:

.. code-block:: bash

   hasura clone hasura/base

This will 'clone' a base project from `hasura.io/hub <https://hasura.io/hub>`_.
Note, you can clone any project from the hub and use that as a starting point for your new project.

.. admonition:: Note

   ``hasura/hello-world`` is another project that contains a few database
   migrations, some sample data and even a sample mircoservice to get started quickly.


Next: Create a Hasura cluster
-----------------------------

Next, let's head to :doc:`Part III: Create a Hasura cluster<3-hasura-cluster>`.
