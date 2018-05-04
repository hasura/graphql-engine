.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Hasura Data
===========

The Hasura ``Data microsevice`` provides GraphQL as well as JSON APIs which run on top of a
:doc:`Postgres database <../postgres/index>` to store and retrieve data.

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
  Running SQL commands <run-sql>
  Permissions and access control <permissions>
  relationships/index
  link-auth-users
  data-migration
  Views <views>
  Aggregations <aggregations>
  Altering the schema <alter-schema/index>
  data-modelling/index
