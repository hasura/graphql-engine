Enum type fields
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Enum type fields can only take a value from a fixed set of allowed values.

In a relational database such as Postgres, an enum type field in a table can be defined by:

- using native database enum types
- setting a foreign key to a reference table which contains the list of allowed values

`Postgres Enum types <https://www.postgresql.org/docs/current/datatype-enum.html>`__ are not easily mutable. Hence
they should be used only for enums which are not going to change over time. e.g. measurement units, days of the
week, etc.

For enums whose values are dynamic and will require updates, the reference table approach is recommended. e.g. list
of tags, list of teams, etc.

.. admonition:: Current limitations

  Hasura currently does not generate GraphQL enums. This feature is being worked upon. Hence this guide is currently
  only tailored towards helping you maintain data consistency in your database.


**For example**, let's say we have a table ``magazine`` with fields ``(id, title, issue_month, issue_year)``
and we would like to restrict the values of the ``issue_month`` field to just the months of the year (i.e. January,
February, and so on).

The following are the approaches we can use to achieve this:

Option 1: Using native Postgres enum type
-----------------------------------------

Create a Postgres enum type
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Open the Hasura console and head to the ``Data -> SQL`` interface.

Run the following SQL statement:

.. code-block:: sql

  CREATE TYPE month AS ENUM ('January', 'February', 'March', 'and so on...');

Set column type as the Postgres enum type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Run the following SQL statement if the table doesn't yet exist:

.. code-block:: sql
  :emphasize-lines: 4

  CREATE TABLE magazine(
    id serial PRIMARY KEY,
    title text NOT NULL,
    issue_month month,
    issue_year integer
  );

If table exists, run the following SQL statement:

.. code-block:: sql

  ALTER TABLE magazine
    ALTER COLUMN issue_month TYPE month using issue_month::month;


Now the ``issue_month`` field can only take values from the months of the year.

See `Postgres Enum types documentation <https://www.postgresql.org/docs/current/datatype-enum.html>`__ for more info.

Option 2: Using a reference table
---------------------------------

Create a reference table for the enum
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Open the Hasura console and head to the ``Data -> Create table`` interface.

Create a table ``months_of_the_year`` with just one column ``month``, which is the primary key:

.. thumbnail:: ../../../img/graphql/manual/schema/enum-create-ref-table.png

Add the allowed enum values to the reference table
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Head to the ``GraphiQL`` tab of the console and run an insert mutation to insert the allowed enum values:

.. thumbnail:: ../../../img/graphql/manual/schema/enum-insert-ref-values.png

Add a foreign-key constraint to the reference table
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Head to the ``Data -> magazine -> Modify`` tab of the console and set a foreign-key to the ``months_of_the_year`` table
using the fields: ``issue_month -> months_of_the_year :: month``:

.. thumbnail:: ../../../img/graphql/manual/schema/enum-set-foreign-key.png

Now the ``issue_month`` field can only take values from the months of the year.
