Setting up schema using an existing database
============================================

When you have an existing database with a schema already present, you don't need to create tables or views or run
DDL through the Hasura console.

All you need to do is indicate to Hasura GraphQL engine, which tables and views you want to expose over GraphQL and
how they are connected to each other so that you can query them as a "graph".

Track tables
------------

Tracking a table or a view means telling Hasura GraphQL engine that you want to expose that table/view over
GraphQL. To track a table or a view:

#. Head to the ``Data -> Schema`` section of the console.
#. Under the heading ``Untracked Tables/Views``, click on the ``Add`` button next to the table/view name.
#. You can also track all the tables and views present in the database by clicking the ``Add all`` button.

Track foreign-keys
------------------

Tracking a foreign key means automatically creating a relationship based on the foreign key.

To track the foreign keys of all tables in the database:

#. Head to the ``Data -> Schema`` section of the console.
#. Under the heading ``Untracked Relations``, click on the ``Track Available Relations`` to automatically create
   relationships based on the foreign keys.

Hasura GraphQL engine also automatically generates relationship names based on the table names and the foreign key
names. The name is generated in the following format:

- For object relationships,

  .. code-block:: none

      Camel case of (foreignTableName + By + columnName)

- For array relationships,

  .. code-block:: none

      Camel case of (foreignTableName + s + By + foreignKeyOfForeigntable)


Read more about relationships :doc:`here <relationships>`.
