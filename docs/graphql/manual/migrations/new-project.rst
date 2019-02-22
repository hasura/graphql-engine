Migrations for a new project
============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide will help you if you are about to start setting up your schema from scratch. You can use migrations
to help track the database and GraphQL schema changes.

These are the steps you need to follow:

#. Install the Hasura CLI
#. Setup a project directory
#. Use the Hasura CLI console (``http://localhost:9695``) instead of the console served by the
   GraphQL engine (E.g.: ``http://my-graphql.herokuapp.com``) to set the schema up


Step 0: Take a note of your  GraphQL engine endpoint
----------------------------------------------------

Let's say you've deployed the GraphQL engine on Heroku, then this endpoint is: ``http://my-graphql.herokuapp.com``.
In case you've deployed this using Docker the URL might be ``http://xx.xx.xx.xx:8080``.

Step 1: Install the Hasura CLI
------------------------------

Follow the instructions in :doc:`../hasura-cli/install-hasura-cli`

Step 2: Setup a project directory
---------------------------------

Skip this step if you already have a project directory.

.. code-block:: bash

  hasura init --directory my-project --endpoint http://my-graphql.herokuapp.com

Step 3: Use the console from the CLI
------------------------------------

Instead of using the console at ``http://my-graphql.herokuapp.com/console`` you should now use the console by running:

.. code-block:: bash

   # Without admin secret key
   hasura console

   # With admin secret key
   hasura console --admin-secret adminsecretkey

Step 4: Add a new table and see how a migration is added
--------------------------------------------------------

As you use the Hasura console UI to make changes to your schema, migration files are automatically generated
in the ``migrations/`` directory in your project.


Step 5: Apply the migrations to another instance of the GraphQL engine
----------------------------------------------------------------------

- Edit ``config.yaml`` and change the endpoint to another instance, say ``https://my-another-graphql.herokuapp.com``:

  .. code-block:: yaml

     # config.yaml
     endpoint: https://my-another-graphql.herokuapp.com

- Apply all migrations present in the ``migrations/`` directory on this new instance:

  .. code-block:: bash

     hasura migrate apply

Step 6: Create migrations without the console & other advanced actions
----------------------------------------------------------------------

- Each migration consists of a pair of yaml and sql files with up and down steps.
- Create migrations manually using :ref:`migrate create <hasura_migrate_create>`.
- You can apply only certain versions or numbers of steps. Read more at :ref:`migrate apply <hasura_migrate_apply>`
