Migrations for an existing database and Hasura instance
=======================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide is to be followed if you already have set up a database and Hasura
instance and now want to start using migrations to help you track the database
and GraphQL schema changes.

Step 0: Take a note of your GraphQL engine endpoint
---------------------------------------------------

Let's say you've deployed the GraphQL engine on Heroku, then this endpoint is:
``https://my-graphql.herokuapp.com``.
In case you've deployed this using Docker the URL might be
``http://xx.xx.xx.xx:8080``.

.. note::

   This endpoint should not contain the ``v1alpha1/graphql`` API path. It should
   just be the hostname and any sub-path if it is configured that way.

Step 1: Install the Hasura CLI
------------------------------

Follow the instructions in :ref:`install_hasura_cli`.

Step 2: Set up a project directory
----------------------------------

.. code-block:: bash

  hasura init --directory my-project --endpoint http://my-graphql.herokuapp.com

Step 3: Initialize the migrations as per your current state
-----------------------------------------------------------

- Use ``pg_dump`` to export the database schema. We're using the ``pg_dump``
  command bundled within the ``postgres`` docker container. If you have
  ``pg_dump`` installed on your machine, you could use that as well.

  .. code-block:: bash

     # get the container id for postgres
     docker ps

     # dump the public schema into public-schema.sql (repeat for other schemas)
     docker exec <postgres-container-id> pg_dump -U postgres --schema-only --schema public > public-schema.sql

  .. note::

     It is safe to remove the initial ~20 lines which are ``SET`` statements
     including ``SELECT pg_catalog.set_config('search_path', '', false);`` from
     the SQL file and keep only the ``CREATE`` like statements that follow.

WIP

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
