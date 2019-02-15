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

Follow the instructions in :doc:`../hasura-cli/install-hasura-cli`

Step 2: Set up a project directory
----------------------------------

Skip this step if you already have a project directory.

.. code-block:: bash

  hasura init --directory my-project --endpoint http://my-graphql.herokuapp.com

Step 3: Open the console via the CLI & disable Postgres schema changes
----------------------------------------------------------------------

Instead of using the console at ``http://my-graphql.herokuapp.com/console`` you xshould now use the console by running:

.. code-block:: bash

   # Without admin secret key
   hasura console

   # With admin secret key
   hasura console --admin-secret-key myadminsecretkey

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
