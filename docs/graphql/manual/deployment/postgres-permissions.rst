Postgres permissions
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

If you're running in a controlled environment, you might need to configure Hasura GraphQL engine to use a
specific Postgres user that your DBA gives you.

Hasura GraphQL engine needs access to your Postgres database with the following permissions:

- (required) Read & write access on 2 schemas: ``hdb_catalog`` and ``hdb_views``.
- (required) Read access to the ``information_schema`` and ``pg_catalog`` schemas, to query for list of tables.
- (required) Read access to the schemas (public or otherwise) if you only want to support queries.
- (optional) Write access to the schemas if you want to support mutations as well
- (optional) To create tables and views via the Hasura console (the admin UI) you'll need the privilege to create
  tables/views. This might not be required when you're working with an existing database


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

    -- Similarly add this for other schemas, if you have any.
    -- GRANT USAGE ON SCHEMA <schema-name> TO hasurauser;
    -- GRANT ALL ON ALL TABLES IN SCHEMA <schema-name> TO hasurauser;
    -- GRANT ALL ON ALL SEQUENCES IN SCHEMA <schema-name> TO hasurauser;
