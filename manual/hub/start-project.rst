.. _hub:

==============================
Start with a project from /hub
==============================

Head to `hasura.io/hub <https://hasura.io/hub>`_ and choose a project that you want to start with.
For eg: ``user/my-nodejs-project``.

Here are some useful projects to start with:

1. ``hasura/hello-*``: Projects that are published by the ``hasura`` user are projects that we have published for different frameworks to help you get started.
2. ``hasura/base``: Use the base project to start off on a completely empty project. You can add your own microservices, schemas and configurations to this.


Option 1: Clone the project and deploy it to a new cluster
----------------------------------------------------------

First, :doc:`install <../install-hasura-cli>` the hasura CLI.

Then, run the following instructions on your terminal or command line:

.. code-block:: bash

   hasura quickstart user/my-nodejs-project
   cd my-nodejs-project
   git add . && git commit -m 'Initial commit'
   git push hasura master

Once your `git push` succeeds, everything is deployed to a new free Hasura cluster.

Now you can add/remove/modify:

1. microservices: In the ``microservices/`` directory
2. schema: Run ``hasura api-console`` and head to the ``Data`` tab. You can browse, modify the schema appropriately.
2. conf: Modify the appropriate files in the ``conf/`` directory.


Option 2: Clone the project without deploying it
------------------------------------------------

You might want to clone a project without deploying it for a few reasons:

1. You want to deploy it on a cluster you already have
2. You want to clone the source code of the project as reference

First, :doc:`install <../install-hasura-cli>` the hasura CLI.

Then, run the following instructions on your terminal or command line:

.. code-block:: bash

   hasura clone user/my-nodejs-project
   cd my-nodejs-project

Now, you'll have the project source code on your computer.
