Postgres credentials needed by Hasura
======================================

If you're running in a controlled environment, you might need to configure Hasura to use a specific postgres user that your DBA gives you.

Hasura needs access to your postgres database with the following permissions:

- (required) Create, read & write access on 2 schemas: ``hdb_catalog`` and ``hdb_views``

  - Note that you can create these 2 schemas and then provide read/write access to the postgres user that you configure Hasura to use
- (required) Read access to the postgres schemas (public or otherwise) if you only want to support queries
- (optional) Write access to the postgres schemas if you want to support mutations as well
- (optional) To create tables and views via the Hasura console (the admin UI) you'll need the privilege to create tables/views. This might not be required when you're working with an existing database
