.. _setup_migrations:

Setting up Hasura migrations
============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Initializing migrations
-----------------------

.. code-block:: bash

  hasura init

  hasura migrate create --from-server <endpoint>
  hasura metadata export --from-server <endpoint>

Generating migrations
---------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    - Open the Hasura console via the CLI

      .. code-block:: bash

        hasura console

    - As you make changes, migration files will be created and latest
      metadata will be exported automatically

  .. tab:: Manually

    - Create a new migration

      .. code-block:: bash

        hasura migrate create <name-of-migration>

    - Add SQL manually to the ``up.sql`` and ``down.sql`` files in the created
      migration directory
    - Edit the corresponding metadata manually

Managing migrations
-------------------

For maintaining a clean set of migrations with the possibility to move between
different checkpoints in your DB and metadata state it is recommended to clean
up intermediate DB migration files and to version control the Hasura project.

Squash migrations
^^^^^^^^^^^^^^^^^

Typically while adding a feature a lot of incremental migration files get
created for each of the small tasks that you did to achieve the feature.

Once you are confident about the final state of a feature, you can use the
``migrate squash`` command to make a single DB migration file containing all
the intermediate steps required to reach the final state.

.. code-block:: bash

  hasura migrate squash --name "<feature-name>" --from <migration-version>

Add checkpoints
^^^^^^^^^^^^^^^

As your metadata is exported on every change you make to the schema, once a final
state for a feature is reached you should mark it as a checkpoint via version
control so that you can get back the metadata at that point.

.. code-block:: bash

  git commit -m "<feature-name>"

Applying migrations
-------------------

- Get the Hasura project with the ``migrations`` and ``metadata`` directories.

- Apply DB migration files and metadata snapshot

  .. code-block:: bash

    hasura migrate apply --endpoint <server-endpoint>
    hasura metadata apply --endpoint <server-endpoint>

Your Hasura server should be up and running!