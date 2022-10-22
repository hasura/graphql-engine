.. meta::
   :description: Postgres permissions for Hasura Cloud projects
   :keywords: hasura cloud, docs, deployment, postgres, postgres permissions

.. _cloud_postgres_permissions:

Postgres permissions
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Hasura works out of the box with the default superuser, usually called ``postgres``, created by most managed cloud database providers.

If you use another database user, you will need to make sure that this user has the right Postgres permissions.

Postgres permissions
--------------------

Here's a sample SQL block that you can run on your database (as a **superuser**) to create the right credentials for a sample Hasura user called ``hasurauser``:

.. code-block:: sql

    -- create a separate user for hasura (if you don't already have one)
    CREATE USER hasurauser WITH PASSWORD 'hasurauser';

    -- create pgcrypto extension, required for UUID
    CREATE EXTENSION IF NOT EXISTS pgcrypto;

    -- create the schemas required by the hasura cloud system
    CREATE SCHEMA IF NOT EXISTS hdb_catalog;
    CREATE SCHEMA IF NOT EXISTS hdb_views;
    CREATE SCHEMA IF NOT EXISTS hdb_pro_catalog;

    -- make the user an owner of the hasura cloud system schemas
    ALTER SCHEMA hdb_catalog OWNER TO hasurauser;
    ALTER SCHEMA hdb_views OWNER TO hasurauser;
    ALTER SCHEMA hdb_pro_catalog OWNER TO hasurauser;

    -- grant select permissions on information_schema and pg_catalog
    GRANT SELECT ON ALL TABLES IN SCHEMA information_schema TO hasurauser;
    GRANT SELECT ON ALL TABLES IN SCHEMA pg_catalog TO hasurauser;

    -- grant all privileges on all tables in the public schema (this is optional and can be customized)
    GRANT USAGE ON SCHEMA public TO hasurauser;
    GRANT ALL ON ALL TABLES IN SCHEMA public TO hasurauser;
    GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO hasurauser;
    GRANT ALL ON ALL FUNCTIONS IN SCHEMA public TO hasurauser;

    -- Similarly add these for other schemas as well, if you have any
    -- GRANT USAGE ON SCHEMA <schema-name> TO hasurauser;
    -- GRANT ALL ON ALL TABLES IN SCHEMA <schema-name> TO hasurauser;
    -- GRANT ALL ON ALL SEQUENCES IN SCHEMA <schema-name> TO hasurauser;
    -- GRANT ALL ON ALL FUNCTIONS IN SCHEMA <schema-name> TO hasurauser;

You may notice the following commands throw warnings/errors:

.. code-block:: sql

  postgres=> GRANT SELECT ON ALL TABLES IN SCHEMA information_schema TO hasurauser;
  WARNING:  no privileges were granted for "sql_packages"
  WARNING:  no privileges were granted for "sql_features"
  WARNING:  no privileges were granted for "sql_implementation_info"
  ERROR:  permission denied for table sql_parts

  postgres=> GRANT SELECT ON ALL TABLES IN SCHEMA pg_catalog TO hasurauser;
  ERROR:  permission denied for table pg_statistic

You can **ignore** these warnings/errors or skip granting these permission as usually all users have relevant access to ``information_schema`` and ``pg_catalog`` schemas by default (see keyword `PUBLIC <https://www.postgresql.org/docs/current/sql-grant.html>`_).

.. note::

   If you first connect Postgres with the default superuser, and afterwards with another user, you might get an error.
   You then need to reset the permissions to the new user.

Note for GCP
^^^^^^^^^^^^^

On Google Cloud SQL, if you are creating a new user and giving the :ref:`above <postgres_permissions>` privileges, 
then you may notice that the following commands may throw warnings/errors:

.. code-block:: sql

   postgres=> ALTER SCHEMA hdb_catalog OWNER TO hasurauser;
   ERROR:  must be member of role "hasurauser"

This happens because the superuser created by the cloud provider sometimes has different permissions. To fix this, you can run the following command first:

.. code-block:: sql

   -- assuming "postgres" is the superuser that you are running the commands with.
   postgres=> GRANT hasurauser to postgres;
   GRANT
   postgres=> ALTER SCHEMA hdb_catalog OWNER TO hasurauser;

Further reading
---------------

For more information and a more detailed explanation on Postgres permissions, refer to the :ref:`Hasura core Postgres requirements <postgres_requirements>` page.
