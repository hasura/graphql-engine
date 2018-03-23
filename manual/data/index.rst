.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Database
========

Hasura comes with data APIs which run on top of a **Postgres database** to store and retrieve data.

To fetch associated data with ease, one can define :doc:`relationships <relationships>` on
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

  create-table
  insert
  select
  update
  delete
  Bulk queries <bulk>
  Linking to auth users <link-auth-users>
  Permissions and access control <permissions>
  relationships
  data-migration
  Altering the schema <alter-schema>
  Aggregations/Views <aggregations>
  reset
  Running SQL commands <run-sql>
  Accessing postgres directly <access-postgres>
  Importing data from SQL files <import-sql>
  data-modelling
  graphql
