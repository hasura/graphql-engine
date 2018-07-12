Existing database
=================

This guide will help you setup migrations in case you're working with an existing application or database that already has database migration tooling.

In this case, you'll be using Hasura only to manage changes to the Hasura metadata which affect your GraphQL schema and not the underlying database.

Step 0: Take a note of your current GraphQL engine endpoint
-----------------------------------------------------------

Let's say you've deployed the GraphQL engine on Heroku, then this endpoint is: ``http://my-graphql.herokuapp.com``.
In case you've deployed this on a VM the URL might be ``http://xx.xx.xx.xx:8080``.

Step 1: Install the Hasura CLI
------------------------------

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Mac

      In your terminal enter the following command:

      .. code-block:: bash

         curl -L https://cli.hasura.io/install.sh | bash

      This will install the ``hasura`` CLI in ``/usr/local/bin``. You might have to provide
      your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

   .. tab:: Linux

      Open your linux shell and run the following command:

      .. code-block:: bash

         curl -L https://cli.hasura.io/install.sh | bash

      This will install the ``hasura`` CLI tool in ``/usr/local/bin``. You might have to provide
      your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

   .. tab:: Windows

      .. note::

         You should have ``git bash`` installed to use ``hasura`` CLI. Download git bash using the following `(link)
         <https://git-scm.com/download/win>`_. Also, make sure you install it in ``MinTTY`` mode, instead of Windows'
         default console.

      Download the ``hasura`` installer:

      * `hasura (64-bit Windows installer) <https://cli.hasura.io/install/windows-amd64>`_
      * `hasura (32-bit Windows installer) <https://cli.hasura.io/install/windows-386>`_

      **Note:** Please run the installer as Administrator to avoid PATH update errors. If you're still
      getting a `command not found` error after installing Hasura, please restart Gitbash.


Step 2: Setup a project directory
---------------------------------
Skip this step if you already have a project directory.

.. code-block:: bash

  hasura init --directory my-project --endpoint http://my-graphql.herokuapp.com

Step 4: Open the console via the CLI & disable postgres schema changes
-----------------------------------------------------------------------

Instead of using the console at ``http://my-grapqhl.herokuapp.com/console`` you should now use the console by running:

.. code-block::

   #Without access key
   hasura console

   #With access key
   hasura console --access-key mysecretkey


Step 5: Untract a table, or modify a relationship/permission
------------------------------------------------------------

As you use the console to track/untrack tables, views or update relationships and permissions you'll see how the metadata file changes
automatically at ``migrations/metadata.yaml``.

Step 6: Apply the metadata.yaml file to another instance of GraphQL engine
--------------------------------------------------------------------------

.. note:: TODO

   Shahidh/Arvi please fill this

Step 7: Refer to the ``hasura metadata`` CLI reference
------------------------------------------------------

To clear, export, apply metadata refer to

.. note:: TODO

   Shahidh/Arvi please fill this
