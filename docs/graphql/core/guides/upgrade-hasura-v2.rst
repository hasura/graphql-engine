.. meta::
   :description: Upgrading to Hasura GraphQL engine v2
   :keywords: hasura, docs, guide, compatibility

.. _upgrade_hasura_v2:

Upgrading to Hasura GraphQL engine v2
=====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This page talks about the conceptual changes introduces in Hasura v2 and things to
keep in mind while upgrading from Hasura v1 to v2.

Do reach out to us if you run into any issues while using Hasura v2 or have any questions
regarding any changes introduced.

What has changed?
-----------------

Concepts
^^^^^^^^

The following are the most significant conceptual changes introduced in Hasura v2:

- Hasura v2 can now connect to multiple databases to generate a unified GraphQL API. Each connected
  database will have a unique "source name" that is used to identify it. Apart from Postgres, connecting to
  SQL server databases is also now supported.

- Config for connecting a database used to be a startup configuration (i.e. set via env vars/flags)
  but since v2, it is a part of Hasura metadata and can be managed dynamically.
  See :ref:`connecting databases <connect_database>`.

- Hasura metadata can now be stored in a separate dedicated Postgres DB.

.. note::

  A detailed changelog with all the new features introduced in Hasura v2 is available on the
  `releases page <https://github.com/hasura/graphql-engine/releases>`__.

Hasura configuration
^^^^^^^^^^^^^^^^^^^^

- To accommodate changes for storing information for multiple databases, the Hasura metadata and
  the Hasura CLI project versions have been bumped from ``v2`` to ``v3``. The ``v2`` versions of the
  metadata and CLI project can continue to be used with Hasura v2 instances. Hasura v2 will assume the
  ``v2`` metadata and migrations belong to a database connected with the name ``default``.

- A new optional env var ``HASURA_GRAPHQL_METADATA_DATABASE_URL`` is now introduced. When set, this
  Postgres database is used to store the Hasura metadata. If not set, the database set using
  ``HASURA_GRAPHQL_DATABASE_URL`` is used to store the Hasura metadata.

  Either one of ``HASURA_GRAPHQL_METADATA_DATABASE_URL`` or ``HASURA_GRAPHQL_DATABASE_URL`` needs to be set
  with a Postgres database to start a Hasura v2 instance as Hasura always needs a Postgres database to store
  its metadata.

- The database set using the ``HASURA_GRAPHQL_DATABASE_URL`` env var is connected automatically with the name
  ``default`` in Hasura v2 while upgrading an existing instance or while starting a fresh instance.

  Setting this env var post initial setup/upgrade will have no effect as the Hasura metadata for data sources would already
  have been initialized and the env var will be treated as any other custom env var.

  It is now not mandatory to set this env var if a dedicated ``HASURA_GRAPHQL_METADATA_DATABASE_URL`` is set.

- The values of the env vars ``HASURA_GRAPHQL_PG_CONNECTIONS``, ``HASURA_GRAPHQL_PG_TIMEOUT`` and ``HASURA_GRAPHQL_NO_OF_RETRIES``
  are used to define the connection parameters of the ``default`` database while upgrading an existing instance
  or while starting a fresh instance.

  Post initial setup/upgrade, these env vars can be considered as Deprecated. Changing or setting values of these env vars
  will have no impact as the values in the Hasura metadata are now used to define the connection parameters.

- Custom env vars can now be used to connect databases dynamically at runtime.

Hasura Cloud
^^^^^^^^^^^^

Hasura Cloud projects' metadata is now stored in metadata DBs managed by Hasura Cloud. Hence
the ``HASURA_GRAPHQL_METADATA_DATABASE_URL`` env var is not configurable on Hasura Cloud and is managed
by Hasura Cloud itself.

By default Hasura Cloud projects are created without any databases connected to them. See :ref:`connecting databases <connect_database>`
to add a database to a Hasura Cloud v2 project.

See the below section on :ref:`hasura_v1_v2_compatibility` to use a Hasura v2 Cloud project like a Hasura v1
Cloud project.

Moving from Hasura v1 to Hasura v2
----------------------------------

.. _hasura_v1_v2_compatibility:

Hasura v1 and Hasura v2 compatibility
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

All existing metadata and migrations from a Hasura v1 instance are assumed to belong to a database named ``default`` in
Hasura v2.

Hence **in Hasura v2, a database with name "default" needs to be added to apply metadata and migrations from a
Hasura v1 instance**.

Post adding a database named ``default``, the Hasura v2 instance should behave equivalently to the Hasura
v1 instance and all previous workflows will continue working as they were.

Refer to :ref:`connecting databases <connect_database>` to add a database to Hasura v2.


Migrate Hasura v1 instance to Hasura v2
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Hasura v2 is backwards compatible with Hasura v1. Hence simply updating the Hasura docker image version number
and restarting your Hasura instance should work seamlessly. The database connected using the ``HASURA_GRAPHQL_DATABASE_URL``
env var will be added as a database with the name ``default`` automatically and all existing metadata and migrations will be
assumed to belong to it.

Upgrade CLI project to enable multiple database support
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Update your Hasura CLI project to ``config v3`` using the steps mentioned in :ref:`this guide <migrations_upgrade_v3>`
to take full advantages of the features introduced in Hasura v2.

Post upgrading to ``config v3``, the database connection parameters would have been moved to the metadata. Hence it is important
to ensure that the same env vars are used for storing database connection strings across all environments and the metadata
being applied also uses the appropriate env vars.

.. note::

  If you do not need multiple database support then you can continue to use ``config v2`` project directory and workflows.

  Though we would recommend to upgrade to ``config v3`` anyway as it includes some useful directory structure changes.

Updates to CI/CD after upgrading to Hasura v2
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following commands need to be executed in the specified order to apply metadata and migrations in CI/CD workflows

- If using Hasura CLI project in ``config v2``:

  - No changes needed.

  - Run:

    - ``hasura migrate apply`` - *(apply migrations to the database named "default")*
    - ``hasura metadata apply`` - *(apply metadata to the database named "default")*


- If using Hasura CLI project in ``config v3``:

  - Ensure that the same env vars are used for storing database connection strings across all environments and the metadata
    being applied also uses the appropriate env vars.

  - Run:

    - ``hasura metadata apply`` - *(connect Hasura to the databases configured in the metadata)*
    - ``hasura migrate apply --all-databases`` - *(apply the migrations to the connected databases)*
    - ``hasura metadata reload`` - *(make Hasura aware of any newly created database objects in the previous step)*
