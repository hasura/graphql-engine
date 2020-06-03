.. meta::
   :description: Postgres requirements for Hasura GraphQL engine
   :keywords: hasura, docs, deployment, postgres, postgres permissions, postgres version

.. _postgres_requirements:

Postgres requirements
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

.. _postgres_version_support:

Supported Postgres versions
---------------------------

Hasura GraphQL engine supports **Postgres versions 9.5 and above**.

Feature requirements
^^^^^^^^^^^^^^^^^^^^

- :ref:`Hasura actions <actions>` are supported in Postgres 10 and above.

.. _postgres_permissions:

Postgres permissions
--------------------

If you're running in a controlled environment, you might need to configure the Hasura GraphQL engine to use a
specific Postgres user that your DBA gives you.

The Hasura GraphQL engine needs access to your Postgres database with the following permissions:

- (required) Read & write access on 2 schemas: ``hdb_catalog`` and ``hdb_views``.
- (required) Read access to the ``information_schema`` and ``pg_catalog`` schemas, to query for list of tables.
- (required) Read access to the schemas (public or otherwise) if you only want to support queries.
- (optional) Write access to the schemas if you want to support mutations as well.
- (optional) To create tables and views via the Hasura console (the admin UI) you'll need the privilege to create
  tables/views. This might not be required when you're working with an existing database.


Here's a sample SQL block that you can run on your database to create the right credentials:

.. code-block:: sql

    -- We will create a separate user and grant permissions on hasura-specific
    -- schemas and information_schema and pg_catalog
    -- These permissions/grants are required for Hasura to work properly.

    -- create a separate user for hasura
    CREATE USER hasurauser WITH PASSWORD 'hasurauser';

    -- create pgcrypto extension, required for UUID
    CREATE EXTENSION IF NOT EXISTS pgcrypto;

    -- create the schemas required by the hasura system
    -- NOTE: If you are starting from scratch: drop the below schemas first, if they exist.
    CREATE SCHEMA IF NOT EXISTS hdb_catalog;
    CREATE SCHEMA IF NOT EXISTS hdb_views;

    -- make the user an owner of system schemas
    ALTER SCHEMA hdb_catalog OWNER TO hasurauser;
    ALTER SCHEMA hdb_views OWNER TO hasurauser;

    -- grant select permissions on information_schema and pg_catalog. This is
    -- required for hasura to query list of available tables
    GRANT SELECT ON ALL TABLES IN SCHEMA information_schema TO hasurauser;
    GRANT SELECT ON ALL TABLES IN SCHEMA pg_catalog TO hasurauser;

    -- Below permissions are optional. This is dependent on what access to your
    -- tables/schemas - you want give to hasura. If you want expose the public
    -- schema for GraphQL query then give permissions on public schema to the
    -- hasura user.
    -- Be careful to use these in your production db. Consult the postgres manual or
    -- your DBA and give appropriate permissions.

    -- grant all privileges on all tables in the public schema. This can be customised:
    -- For example, if you only want to use GraphQL regular queries and not mutations,
    -- then you can set: GRANT SELECT ON ALL TABLES...
    GRANT USAGE ON SCHEMA public TO hasurauser;
    GRANT ALL ON ALL TABLES IN SCHEMA public TO hasurauser;
    GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO hasurauser;
    GRANT ALL ON ALL FUNCTIONS IN SCHEMA public TO hasurauser;

    -- Similarly add this for other schemas, if you have any.
    -- GRANT USAGE ON SCHEMA <schema-name> TO hasurauser;
    -- GRANT ALL ON ALL TABLES IN SCHEMA <schema-name> TO hasurauser;
    -- GRANT ALL ON ALL SEQUENCES IN SCHEMA <schema-name> TO hasurauser;

**pgcrypto** in PG search path
------------------------------

Hasura GraphQL engine needs the ``pgcrypto`` Postgres extension to function.

During initialization, Hasura GraphQL engine tries to install the ``pgcrypto`` extension
in the ``public`` schema, if it is not already installed.

It needs to be ensured that ``pgcrypto`` is installed in a schema which is in the Postgres
`search path <https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATH>`_
for the Postgres user/role that Hasura connects with.

If ``pgcrypto`` is installed in a schema that is not in the search path, the
schema can be added to the search path by executing one of the following SQL commands
depending on your setup:

.. code-block:: sql

    -- set search path to include schemas for the entire database
    ALTER DATABASE <database_name> SET search_path TO schema1,schema2;

    -- OR --

    -- set search path to include schemas for a particular role
    ALTER ROLE <hasura_role> SET search_path TO schema1,schema2;
