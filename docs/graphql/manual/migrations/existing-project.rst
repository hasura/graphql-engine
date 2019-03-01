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

Follow the instructions in :doc:`../hasura-cli/install-hasura-cli`

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

   # Without admin secret key
   hasura console

   # With admin secret key
   hasura console --admin-secret myadminsecretkey

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
