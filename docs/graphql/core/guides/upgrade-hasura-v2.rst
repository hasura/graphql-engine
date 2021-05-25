.. meta::
   :description: Updating to Hasura GraphQL engine v2
   :keywords: hasura, docs, guide, compatibility, update v2

.. _upgrade_hasura_v2:

Updating to Hasura GraphQL engine v2
====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This page talks about the conceptual changes introduces in Hasura v2 and things to
keep in mind while updating from Hasura v1 to v2.

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

.. _hasura_v2_behaviour_changes:

Breaking behaviour changes
^^^^^^^^^^^^^^^^^^^^^^^^^^

- **Semantics of explicit "null" values in "where" filters have changed**

  According to the discussion in `issue 704 <https://github.com/hasura/graphql-engine/issues/704#issuecomment-635571407>`_, an explicit ``null``
  value in a comparison input object will be treated as an error rather than resulting in the expression being evaluated to ``True``.

  For example: The mutation ``delete_users(where: {id: {_eq: $userId}}) { name }`` will yield an error if ``$userId`` is ``null`` instead of deleting
  all users.

  The older behaviour can be preserved by setting the ``HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE`` env var to ``true``.

- **Semantics of "null" join values in remote schema relationships have changed**

  In a remote schema relationship query, the remote schema will be queried when
  all of the joining arguments are not ``null`` values. When there are ``null`` value(s), the remote schema won't be queried and the response of
  the remote relationship field will be ``null``. Earlier, the remote schema was queried with the ``null`` value arguments and the response
  depended upon how the remote schema handled the ``null`` arguments but as per user feedback, this behaviour was clearly not expected.

- **Order of keys in objects passed as "order_by" operator inputs is not preserved**

  The ``order_by`` operator accepts an array of objects as input to allow ordering by multiple fields in a given order, i.e.
  ``[{field1: sortOrder}, {field2: sortOrder}]`` but it is also accepts a single object with multiple keys as an input,
  i.e. ``{field1: sortOrder, field2: sortOrder}``. In earlier versions, Hasura's query parsing logic used to maintain the order of keys in the
  input object and hence the appropriate ``order by`` clauses with the fields in the right order were generated .

  As the `GraphQL spec <http://spec.graphql.org/June2018/#sec-Input-Object-Values>`__ mentions that input object keys are unordered, Hasura v2.0's
  new and stricter query parsing logic doesn't maintain the order of keys in the input object taking away the guarantee of the generated ``order by``
  clauses to have the fields in the given order.

  For example: The query ``fetch_users(order_by: {age: desc, name: asc}) {id name age}`` which is intended to fetch users ordered by their age
  and then by their name is now not guaranteed to return results first ordered by age and then by their name as the ``order_by`` input is passed
  as an object. To achieve the expected behaviour, the following query ``fetch_users(order_by: [{age: desc}, {name: asc}]) {id name age}`` should
  be used which uses an array to define the order of fields to generate the appropriate ``order by`` clause.

- **Incompatibility with older Hasura version remote schemas**

  With v2.0, some of the auto-generated schema types have been extended. For example, ``String_comparison_exp`` has an additional ``regex`` input
  object field. This means if you have a Hasura API with an older Hasura version added as a remote schema then it will have a type conflict. You
  should update all Hasura remote schemas to avoid such type conflicts.

- **Migrations are not executed under a single transaction**

  While applying multiple migrations, in earlier Hasura CLI versions all migration files were run under one transaction block. i.e. if any migration
  threw an error, all the previously successfully executed migrations would be rolled back. With Hasura CLI v2.0, each migration file is run in
  its own transaction block but all the migrations are not executed under one. i.e. if any migration throws an error, applying further migrations
  will be stopped but the other successfully executed migrations up till that point will not be rolled back.

.. _hasura_v2_env_changes:

- **Deprecation of data source specific env vars**

  In v2.0, the values of the following env vars are used to define the connection parameters of the ``default`` database
  while updating an existing instance or while starting a fresh instance. During metadata initialization, their values
  are moved to the metadata of the ``default`` source as defined :ref:`here <PGConfiguration>`.

  - ``HASURA_GRAPHQL_PG_CONNECTIONS``
  - ``HASURA_GRAPHQL_PG_TIMEOUT``
  - ``HASURA_GRAPHQL_NO_OF_RETRIES``
  - ``HASURA_GRAPHQL_PG_CONN_LIFETIME``
  - ``HASURA_GRAPHQL_PG_POOL_TIMEOUT``
  - ``HASURA_GRAPHQL_USE_PREPARED_STATEMENTS``
  - ``HASURA_GRAPHQL_TX_ISOLATION``
  - ``HASURA_GRAPHQL_READ_REPLICA_URLS``
  - ``HASURA_GRAPHQL_CONNECTIONS_PER_READ_REPLICA``

  **Post the initial setup/update once the metadata is initialized, these env vars can be considered as Deprecated.**
  i.e. Changing or setting values of these env vars will have no impact as the values in the Hasura metadata are
  now used to define the connection parameters.

.. _hasura_v2_config_changes:

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
  ``default`` in Hasura v2 while updating an existing instance or while starting a fresh instance.

  Setting this env var post initial setup/update will have no effect as the Hasura metadata for data sources
  would already have been initialized and the env var will be treated as any other custom env var.

  It is now not mandatory to set this env var if a dedicated ``HASURA_GRAPHQL_METADATA_DATABASE_URL`` is set.

- Custom env vars can now be used to connect databases dynamically at runtime.

- With support for multiple data sources, older data source specific env vars have been deprecated.
  :ref:`See details <hasura_v2_env_changes>`

Hasura Cloud
^^^^^^^^^^^^

Hasura Cloud projects' metadata is now stored in metadata DBs managed by Hasura Cloud. Hence
the ``HASURA_GRAPHQL_METADATA_DATABASE_URL`` env var is not configurable on Hasura Cloud and is managed
by Hasura Cloud itself.

By default Hasura Cloud projects are created without any databases connected to them. See
:ref:`connecting databases <connect_database>` to add a database to a Hasura Cloud v2 project.

See the below section on :ref:`hasura_v1_v2_compatibility` to use a Hasura v2 Cloud project like a Hasura v1
Cloud project.

.. _moving_from_hasura_v1_to_v2:

Moving from Hasura v1 to Hasura v2
----------------------------------

.. _hasura_v1_v2_compatibility:

Hasura v1 and Hasura v2 compatibility
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

All existing metadata and migrations from a Hasura v1 instance are assumed to belong to a database named ``default``
in Hasura v2.

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

Updates to CI/CD after updating to Hasura v2
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
