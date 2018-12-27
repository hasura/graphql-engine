Migrations with a database with an existing migration system
============================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide will help you setup Hasura specific migrations in case you're working with an existing database
that already has its database migration tooling.

In this case, you'll be using Hasura migrations only to track changes to the Hasura metadata which affect your
GraphQL schema and not the underlying database.

Step 0: Take a note of your GraphQL engine endpoint
---------------------------------------------------

Let's say you've deployed the GraphQL engine on Heroku, then this endpoint is: ``http://my-graphql.herokuapp.com``.
In case you've deployed this using Docker the URL might be ``http://xx.xx.xx.xx:8080``.

Step 1: Install the Hasura CLI
------------------------------

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Mac

      In your terminal enter the following command:

      .. code-block:: bash

         curl -L https://cli.hasura.io/install.sh | bash

      This will install the Hasura CLI in ``/usr/local/bin``. You might have to provide
      your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

   .. tab:: Linux

      Open your linux shell and run the following command:

      .. code-block:: bash

         curl -L https://cli.hasura.io/install.sh | bash

      This will install the Hasura CLI tool in ``/usr/local/bin``. You might have to provide
      your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

   .. tab:: Windows

      .. note::

         You should have ``git bash`` installed to use Hasura CLI. Download ``git bash`` using this `link
         <https://git-scm.com/download/win>`_. Also, make sure you install it in ``MinTTY`` mode, instead of Windows'
         default console.

      Download the ``hasura`` installer:

      * `hasura (64-bit Windows installer) <https://cli.hasura.io/install/windows-amd64>`_
      * `hasura (32-bit Windows installer) <https://cli.hasura.io/install/windows-386>`_

      **Note:** Please run the installer as ``Administrator`` to avoid PATH update errors. If you're still
      getting a "command not found" error after installing Hasura CLI, please restart ``git bash``.


Step 2: Set up a project directory
----------------------------------
Skip this step if you already have a project directory.

.. code-block:: bash

  hasura init --directory my-project --endpoint http://my-graphql.herokuapp.com

Step 3: Open the console via the CLI & disable Postgres schema changes
----------------------------------------------------------------------

Instead of using the console at ``http://my-graphql.herokuapp.com/console`` you should now use the console by running:

.. code-block:: bash

   # Without access key
   hasura console

   # With access key
   hasura console --access-key mysecretkey

Step 4: Disable database schema modifications
---------------------------------------------

Since you are using other tools to manage database migrations, you should disable the tools on the Hasura console
which modify the database schema to prevent edits to the database schema. But, you can still do actions related to
the GraphQL schema, like tracking a table or creating/editing relationships or modifying permissions, as they are
managed by Hasura metadata.

To disable schema modifications, head to ``Data -> Migrations`` on the console and then
disable the toggle ``Allow postgres schema changes``.

Step 5: Track a table, or modify a relationship/permission
----------------------------------------------------------

As you use the console to track/untrack tables, views or update relationships and permissions you'll see how the
metadata file changes automatically at ``migrations/metadata.yaml``.

Step 6: Apply the metadata to another instance of GraphQL engine
----------------------------------------------------------------

- Edit ``config.yaml`` and change the endpoint to another instance, say ``https://my-another-graphql.herokuapp.com``

  .. code-block:: yaml

     # config.yaml
     endpoint: https://my-another-graphql.herokuapp.com

- Apply metadata present in the ``migrations/metadata.yaml`` on this new instance:

  .. code-block:: bash

     hasura metadata apply

Step 7: Other metadata commands 
-------------------------------

To clear, export, apply metadata refer to :ref:`hasura metadata <hasura_metadata>` command.
