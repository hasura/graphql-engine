Migrations for an existing project
==================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide will help you if you already have set up a schema and now want
to start using migrations to help you track the database and GraphQL schema changes.

These are the steps you need to follow:

#. Install the Hasura CLI
#. Set up a project directory
#. Initialize migrations
#. For further changes, use the Hasura CLI console (``http://localhost:9695``) instead of the console served by the
   GraphQL engine (E.g.: ``http://my-graphql.herokuapp.com``)

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
         <https://git-scm.com/download/win>`__. Also, make sure you install it in ``MinTTY`` mode, instead of Windows'
         default console.

      Download the ``hasura`` installer:

      * `hasura (64-bit Windows installer) <https://cli.hasura.io/install/windows-amd64>`__
      * `hasura (32-bit Windows installer) <https://cli.hasura.io/install/windows-386>`__

      **Note:** Please run the installer as ``Administrator`` to avoid PATH update errors. If you're still
      getting a "command not found" error after installing Hasura CLI, please restart ``git bash``.


Step 2: Set up a project directory
----------------------------------
Skip this step if you already have a project directory.

.. code-block:: bash

  hasura init --directory my-project --endpoint http://my-graphql.herokuapp.com

Step 3: Initialize the migrations as per your current state
-----------------------------------------------------------

- Install ``pg_dump`` (or use Docker) and run the following command to download the public schema as ``public-schema.sql``:

  .. code-block:: bash
  
     pg_dump -O -x -h <db-host> -p <db-port> -U <db-user> -d <db-name> --schema public --schema-only > public-schema.sql

  .. note::

     If the exported file contains ``SELECT pg_catalog.set_config('search_path', '', false);``, remove the whole line.
     This can cause issues later when SQL is run without schema qualifiers, since this statement sets search path to ``''``
     instead of the default ``public`` and ``pg_catalog``.

- Export the metadata (this creates a file ``metadata.yaml``):

  .. code-block:: bash
     
     hasura metadata export

- Create a new migration with a name, say ``first``. This will generate some files in ``migrations/`` directory:

  .. code-block:: bash
  
     hasura migrate create first

- Move the contents of ``public-schema.sql`` to ``migrations/<version>_first.up.sql``
- Move the contents of ``metadata.yaml`` to ``migrations/<version>_first.up.yaml`` with the following content
  (take care of indentation):

  .. code-block:: yaml

     - type: replace_metadata
       args:
         <contents-of-metadata.yaml>

- Remove ``migrations/<version>_first.down.{sql,yaml}`` migration files if you are not adding down migrations for these

.. note::

  Migration version cannot be "0". i.e. the files cannot be of the form ``0_<something>.up.sql``

Step 4: Use the console from the CLI
------------------------------------

Instead of using the console at ``http://my-graphql.herokuapp.com/console`` you should now use the console by running:

.. code-block:: bash

   # Without access key
   hasura console

   # With access key
   hasura console --access-key mysecretkey

Step 5: Add a new table and see how a migration is added
--------------------------------------------------------

As you use the Hasura console UI to make changes to your schema, migration files are automatically generated
in the ``migrations/`` directory in your project.

.. note::

   Migrations are only created when using the console through CLI.

Step 6: Apply the migrations to another instance of the GraphQL engine
----------------------------------------------------------------------

- Edit ``config.yaml`` and change the endpoint to another instance, say ``https://my-another-graphql.herokuapp.com``:

  .. code-block:: yaml

     # config.yaml
     endpoint: https://my-another-graphql.herokuapp.com

- Apply all migrations present in the ``migrations/`` directory on this new instance:

  .. code-block:: bash

     hasura migrate apply

Step 7: Create migrations without the console & other advanced actions
----------------------------------------------------------------------

- Each migration consists of a pair of yaml and sql files with up and down steps.
- Create migrations manually using :ref:`migrate create <hasura_migrate_create>`.
- You can apply only certain versions or numbers of steps. Read more at :ref:`migrate apply <hasura_migrate_apply>`
