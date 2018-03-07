.. .. meta::
   :description: Describing the hasura project directory structure
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl, hasuracli

.. _hasura-create-project:

.. highlight:: bash

Creating a project
==================

You can create a hasura project by cloning a project from the collection on `hasura hub <https://hasura.io/hub>`_

The following will 'clone' the project `hasura/base <https://hasura.io/hub/project/hasura/base>`_ (an empty hasura project) to your system.

.. code:: bash

   $ hasura clone base my-project

      Cloning project...
      ✓ Project cloned directory=<base-dir>/my-project

   # creates a directory called `my-project` and initializes an empty Hasura project


You can clone any project from `hasura hub <https://hasura.io/hub>`_ and use that as a starting point for your new project.

`hasura/hello-world <https://hasura.io/hub/project/hasura/hello-world>`_ is a good starter project that contains a few database
migrations, some sample data and even a sample microservice to help learn about hasura features quickly.

.. note::

  This command automatically initializes a git repository in your project folder