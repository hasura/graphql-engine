Postgres credentials needed by Hasura
======================================

If you're running in a controlled environment, you might need to configure Hasura to use a specific postgres user that your DBA gives you.

Hasura needs access to your postgres database with the following permissions:

- (required) Read & write access on 2 schemas: ``hdb_catalog`` and ``hdb_views``
- (required) Read access to the postgres schemas (public or otherwise) if you only want to support queries
- (optional) Write access to the postgres schemas if you want to support mutations as well
- (optional) To create tables and views via the Hasura console (the admin UI) you'll need the privilege to create tables/views. This might not be required when you're working with an existing database


Here's a sample SQL block that you can run on your database to create the right credentials:

.. code-block:: sql

   -- Create schemas where hasura will store its metadata
   CREATE SCHEMA hdb_catalog;
   CREATE SCHEMA hdb_views;

   -- Set a password here
   CREATE USER hasura WITH PASSWORD hasurapassword;

   -- Grant privilege to read the database objects and the data in the "public" schema
   GRANT hasura PRIVILEGE DATABASE_READ, SELECT on SCHEMA public;
   -- Grant privilege to write data in the "public" schema, this will enable mutations
   GRANT hasura PRIVILEGE INSERT, UPDATE, DELETE SCHEMA public;

   -- Grant privileges to read/write on all schemas
   GRANT hasura PRIVILEGE ALL;
