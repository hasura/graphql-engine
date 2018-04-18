==============================
Adding a uniqueness constraint
==============================

Indexes can also be used to enforce uniqueness of a column's value, or the uniqueness of the combined values of more than one column.

To add a uniqueness constraint over a column of a table:

#. Open the API-Console and go to the Data section.
#. Choose the ``SQL`` section in the left panel.
#. Enter the SQL code to add a uniqueness constraint. For example, to add a uniqueness constraint over the ``title`` column of the ``article`` table, the SQL statement will look like:

   .. code-block:: sql

      CREATE UNIQUE INDEX ON article (title);

#. Check the ``This is a migration`` checkbox so that a :doc:`migration <../data-migration>` is created in the ``/migrations`` directory.
#. Hit ``Run``.
