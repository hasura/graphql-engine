Customise schema with views
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Use cases
---------

You may want to customise your GraphQL schema to:

- Limit scope (i.e. expose only a subset of the columns in a table)
- Fetch derived data (aggregations like *count, average, etc.*) in queries

These kind of use-cases can be supported using database views.

Please see the following pages for details about the above use-cases:

- :doc:`../queries/control-access`
- :doc:`../queries/derived-data`

Creating views
--------------

Views can be created using SQL which can be run in the Hasura console:

- Head to the ``Data -> SQL`` section of the Hasura console
- Enter your `create view SQL statement <https://www.postgresql.org/docs/9.6/static/sql-createview.html>`__
- Select the ``Track this`` checkbox to expose the new view over the GraphQL API
- Hit the ``Run`` button



