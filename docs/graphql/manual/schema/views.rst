Customise schema with views
===========================

You may want to customise your GraphQL schema to:

- Limit scope (i.e. expose only a subset of the columns in a table)
- Fetch derived data (aggregations like *count, average, etc.*) in queries

These use-cases can be supported using database views.

You can create views `using SQL <https://www.postgresql.org/docs/9.6/static/sql-createview.html>`_ which you can
run using the console in the ``Data -> SQL`` section.

Please see the following pages for details about the above use-cases:

- :doc:`../queries/control-access`

- :doc:`../queries/aggregations`


