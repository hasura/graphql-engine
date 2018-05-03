.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Hasura Data
===========

Hasura comes with JSON as well as :doc:`GraphQL <graphql/index>` data APIs which run on top of a **Postgres database**
to store and retrieve data.

To fetch associated data with ease, one can define :doc:`relationships <relationships/index>` on
tables. To authorize the access to data, :doc:`permissions <permissions>` can be added based on
user roles.

**Explore the Data APIs**

Use the API console to browse the various Data APIs.

.. code-block:: bash

  # in the project directory
  $ hasura api-console

See:
^^^^

.. toctree::
  :maxdepth: 1

  GraphQL <graphql/index>
  create-table
  insert
  select
  update
  delete
  Bulk queries <bulk>
  Linking to auth users <link-auth-users>
  Permissions and access control <permissions>
  Data Relationships <relationships/index>
  data-migration
  Altering the schema <alter-schema/index>
  Views <views>
  Aggregations <aggregations>
  reset
  Running SQL commands <run-sql>
  Accessing postgres directly <access-postgres>
  Importing data from SQL files <import-sql>
  exporting-data
  data-modelling/index
