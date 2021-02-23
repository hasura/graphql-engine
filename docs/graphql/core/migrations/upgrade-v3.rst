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

Hasura now allows us to add multiple databases. This solves a variety of use cases.
If we want to take advantage of multiple databases we'll have to use a config v3 project 
which brings some changes to the project directory when compared to config v2.

Let's try to understand what these changes are.

Metadata directory
^^^^^^^^^^^^^^^^^^
The following is a sample metadata directory when using config v3.

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

Currently CLI looks for ``!include`` directives in ``tables`` and ``functions`` keys only.

Migrations Directory & Seeds Directory
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
There are some changes to ``migrations`` and ``seeds`` project directories in config V3. Projects will have child directories
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

The steps to apply migrations and metadata to a new hasura instance will be,

.. code-block:: bash
  
  # first apply metadata, this will populate hasura with configuration of connected databases
  hasura metadata apply
  # now we can apply migrations
  hasura migrate apply --database <database-name>
  # follow it with a metadata reload to make sure hasura is aware of the changes
  hasura metadata reload

The reason why we have to do ``metadata apply`` first instead of ``migrate apply`` (that we are used to doing in config v2) is 
If we do a ``migrate apply`` first then hasura might not be aware about the databases it has to connect to. Earlier we could not start hasura
without a connected database, but now we can.

Also, ``hasura seeds`` and ``hasura migrate`` now accepts a required flag ``--database``

Upgrade steps
-------------

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
