.. meta::
  :description: Hasura Config V3 project
  :keywords: hasura, docs, migration, metadata


.. _migrations_upgrade_v3:

Upgrading to Hasura migrations config v3
========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

What has changed?
-----------------

From ``v2.0.0`` onwards, Hasura allows us to add multiple databases to the same Hasura instance.
If we want to take advantage of multiple databases we'll have to use a ``config v3`` project 
which brings some changes to the project directory when compared to ``config v2``.

``Config v3`` also brings updates to the metadata directory structure to improve version control
and collaboration workflows.To gain these benefits we recommend upgrading to ``config v3`` even
if you do not intend to use multiple databases.

Let's try to understand what these changes are.

Metadata directory
^^^^^^^^^^^^^^^^^^

The following is a sample metadata directory when using ``config v3``.

.. code-block:: bash

  metadata
  ├── actions.graphql
  ├── actions.yaml
  ├── allow_list.yaml
  ├── cron_triggers.yaml
  ├── databases
  │   ├── databases.yaml
  │   └── default
  │       └── tables
  │           ├── public_albums.yaml
  │           ├── public_artists.yaml
  ├── query_collections.yaml
  ├── remote_schemas.yaml
  └── version.yaml


Notice the introduction of a new ``databases`` directory. As the name suggests, this directory will have the
metadata related to all your databases. 

Now let's looks at the contents of ``databases/databases.yaml``

.. code-block:: yaml

  - name: default
    configuration:
      connection_info:
        database_url: <database_url>
        pool_settings:
          idle_timeout: 180
          max_connections: 50
          retries: 1
    tables:
    - "!include public_albums.yaml"
    - "!include public_artists.yaml"
    functions: []

We can see that we have a database called `default`, with it's configuration information and other metadata.
Take a a look at the first element under ``tables`` key. This is a special syntax/directive to hasura CLI to "include"
and inline the contents of a file called ``public_albums.yaml``. The location at which the CLI looks for this file is  
``<project-directory>/metadata/databases/<database-name>/tables``

Therefore when doing a ``hasura metadata apply`` CLI will inline elements of ``tables`` key in ``databases.yaml`` with 
content sourced from ``metadata/databases/default/tables``. This allows managing metadata related a table easier since 
it'll have a file of it's own.

.. note::

  Currently the CLI looks for ``!include`` directives in ``tables`` and ``functions`` keys only.

Migrations directory & Seeds directory
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are some changes to ``migrations`` and ``seeds`` project directories in ``config v3``. Projects will have child directories
corresponding to migrations for each connected database. Look at the following example, the ```migrations`` directory
has a sub directory ``default`` which corresponds to the connected database.

.. code-block:: bash

  migrations
  └── default
    └── 1613987232674_init
        └── up.sql


Changes needed in existing workflows
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

With the introduction of multiple databases and it's configuration being in metadata, brings a change of workflow with it.

The steps to apply migrations and metadata to a new hasura instance will be:

.. code-block:: bash
  
  # first apply metadata, this will populate hasura with configuration of connected databases
  hasura metadata apply
  # now we can apply migrations
  hasura migrate apply --database <database-name>
  # follow it with a metadata reload to make sure hasura is aware of the changes
  hasura metadata reload

The reason why we have to do ``metadata apply`` first instead of ``migrate apply`` (that we are used to doing in ``config v2``) is 
If we do a ``migrate apply`` first then hasura might not be aware about the databases it has to connect to. Earlier we could not start hasura
without a connected database, but now we can.

Also, ``hasura seeds`` and ``hasura migrate`` now accepts a required flag ``--database``.

Upgrade steps
-------------

The latest version Hasura CLI comes with a convenience script used to upgrade your CLI project to use ``config v3``. Note that this process is
completely independent from your Hasura GraphQL engine server update process.

Pre update checklist / notes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

1. ``Config V3`` is expected to be used with Hasura GraphQL engine versions ``v2.0.0-alpha.1`` and above.
2. During the update process CLI uses the server as the source of truth, so make sure your server is upto date.
3. The update process replaces project metadata with metadata on the server.

Step 0: Take a backup
^^^^^^^^^^^^^^^^^^^^^

Make sure you take a backup of your Hasura project before upgrading to ``config v3``.

Step 1: Upgrade to the latest CLI
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Run:

.. code-block:: bash

  hasura update-cli

If you are updating to a ``beta/alpha`` release

Run:

.. code-block:: bash

  hasura update-cli --version <version>

Step 2: Upgrade Hasura project to v3
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In your project directory, run:

.. code-block:: bash

  hasura scripts update-project-v3

Your project directory and ``config.yaml`` should be updated to v3. 

The update script will ask for the name of database the current migrations and seeds correspond to. 
If you are starting hasura with a ``HASURA_GRAPHQL_DATABASE_URL`` then the name of the database should be ``default``.

Continue using config v2
------------------------

It is possible to continue using ``config v2`` with Hasura ``v2.0.0`` and above if you would like to do so.

While using ``config v2`` with Hasura ``v2.0.0`` and above, as we have metadata and migrations for only a single database,
the server assumes that they belong to a database named ``default`` and attempts to apply them to it.

Hence, to continue using ``config v2`` we need to connect a database to Hasura GraphQL engine with the name ``default`` and then run
any metadata and migrations commands.

- If you have connected your database using the ``HASURA_GRAPHQL_DATABASE_URL`` env var, the database will be added with the name
  ``default`` automatically.

- Else you can connect a database with the name ``default`` following the steps :ref:`here <connect_database_v2.0>`.

Post this, the metadata and migration commands should work as usual.

.. admonition:: Additional Resources

  Hasura Database Schema Migrations - `Watch Webinar <https://hasura.io/events/webinar/hasura-database-schema-migrations/?pg=docs&plcmt=body&cta=watch-webinar&tech=>`__.