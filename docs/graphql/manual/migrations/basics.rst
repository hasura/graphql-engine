.. meta::
  :description: Hasura migrations basics
  :keywords: hasura, docs, migration, metadata

.. _migrations_basics:

Hasura migrations basics
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

.. note::

  For ``config v1``, see :ref:`manage_migrations_v1`.

Initializing migrations
-----------------------

:ref:`Install <install_hasura_cli>` the Hasura CLI or :ref:`update <hasura_update-cli>`
to the latest version if current version is ``< v1.2.0``.

Now run:

.. code-block:: bash

  # create a Hasura project
  hasura init

  # cd into project dir

  # Export current Hasura state
  hasura migrate create <init-migration-name> --from-server --endpoint <endpoint>
  hasura metadata export --endpoint <endpoint>

  # mark the init migration as applied on this server
  hasura migrate apply --version "<init-migration-version>" --skip-execution

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

    - Add SQL manually to the ``up.sql`` and ``down.sql`` files in the newly
      created migration's directory in ``/migrations``
    - Edit the corresponding metadata manually in ``/metadata``

Managing migrations
-------------------

For maintaining a clean set of migrations with the possibility to move between
different checkpoints in your project's state it is recommended to clean
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

  # mark the squashed migration as applied on this server
  hasura migrate apply --version "<squash-migration-version>" --skip-execution

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

Checking migrations status
--------------------------

The following command will print out each migration version present in the ``migrations``
directory along with its name, source status and database status.

.. code-block:: bash

   # in project dir
   hasura migrate status

For example,

.. code-block:: bash

   $ hasura migrate status
   VERSION        NAME                           SOURCE STATUS  DATABASE STATUS
   1590493510167  init                           Present        Present
   1590497881360  create_table_public_address    Present        Present

Such a migration status indicates that there are 2 migration versions in the
local directory and both of them are applied on the database.

If ``SOURCE STATUS`` indicates ``Not Present``, it means that the migration
version is present on the server, but not on the current user's local directory.
This typically happens if multiple people are collaborating on a project and one
of the collaborators forgot to pull the latest changes which included the latest
migration files, or another collaborator forgot to push the latest migration
files that were applied on the database. Syncing of the files would fix the
issue.

If ``DATABASE STATUS`` indicates ``Not Present``, it denotes that there are new
migration versions in the local directory which are not applied on the database
yet. Executing ``hasura migrate apply`` will resolve this.