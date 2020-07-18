.. _cloud_postgres_permissions:

Postgres Permissions for Hasura Cloud
=====================================

Hasura Cloud works with Postgres versions 9.5 and above.
But, If you’re running in a controlled environment, you might need to configure 
Hasura Cloud to use a specific Postgres user that your DBA gives you.

Apart from the requirements mentioned in `Hasura Core Postgres Requirements Docs <https://hasura.io/docs/1.0/graphql/manual/deployment/postgres-requirements.html#postgres-permissions>`_,
Hasura Cloud needs the following extra permissions:

- (required) Read and write access to ``hdb_pro_catalog`` schema.

.. code-block:: sql

   -- execute these statements after executing the ones mentioned in Hasura Core docs
   -- create the schemas required by the hasura cloud system
   CREATE SCHEMA IF NOT EXISTS hdb_pro_catalog;
   
   -- make the user an owner of system schemas
   ALTER SCHEMA hdb_pro_catalog OWNER TO hasurauser;
