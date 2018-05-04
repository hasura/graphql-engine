PostgreSQL database
===================

`PostgreSQL <https://www.postgresql.org>`_ is one of the most advanced open source relational database with a great
community around it. Hasura comes with a pre-installed PostgreSQL database which runs as the ``Postgres microservice``
on a cluster. An admin user for the database is also configured by default.

The :doc:`Data microservice <../data/index>` provides JSON and GraphQL APIs over the Postgres database. Also, the ``Data``
section of the :doc:`API console <../api-console/index>` provides a UI to interact with the database.

See:
^^^^

.. toctree::
  :maxdepth: 1

  Accessing postgres <access-postgres>
  import-sql
  exporting-data
  reset
