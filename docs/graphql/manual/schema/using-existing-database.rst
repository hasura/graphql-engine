Setting up GraphQL schema using an existing database
====================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

When you have an existing database with a schema already present, you don't need to create tables or views or run
DDL queries through the Hasura console.

All you need to do is indicate to Hasura GraphQL engine which tables and views you want to expose over GraphQL and
how they are connected to each other so that you can query them as a "graph".

Step 1: Track tables/views
--------------------------

Tracking a table or a view means telling Hasura GraphQL engine that you want to expose that table/view over GraphQL.

To track a table or a view:
^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. Head to the ``Data -> Schema`` section of the console.
#. Under the heading ``Untracked Tables/Views``, click on the ``Add`` button next to the table/view name.

To track all tables and views present in the database:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. Head to the ``Data -> Schema`` section of the console.
#. Under the heading ``Untracked Tables/Views``, click the ``Add all`` button.

Step 2: Track foreign-keys
--------------------------

Tracking a foreign-key means creating a :doc:`relationship <relationships/index>` between the tables involved in the
foreign-key.

To track a foreign-key between two tables in the database:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. Head to the ``Data -> Schema`` section of the console.
#. Click on a table involved in the foreign-key and head to the ``Relationships`` tab.
#. You should see a suggested relationship based on the foreign-key. Click ``Add``, give a name to your relationship
   (this will be the name of the :doc:`nested object <../queries/nested-object-queries>` in the GraphQL query), and
   hit ``Save`` to create the relationship.
#. Repeat with the other table involved in the foreign-key.


To track all the foreign-keys of all tables in the database:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. Head to the ``Data -> Schema`` section of the console.
#. Under the heading ``Untracked Relations``, click on the ``Track Available Relations`` to automatically create
   relationships based on the foreign-keys.

.. note::

  In this case, Hasura GraphQL engine will automatically generate relationship names (the names of the :doc:`nested
  objects <../queries/nested-object-queries>` in the GraphQL query) based on the table names and the foreign-key
  names. The name is generated in the following format:

  - For object relationships: ``Camel case of (foreignTableName + By + columnName)``
  - For array relationships: ``Camel case of (foreignTableName + s + By + columnNameInForeignTable)``

  For example, for the foreign-key ``article::author_id -> author::id``, the relationship names will be
  ``authorByAuthorId`` for ``article`` table and ``articlesByAuthorId`` for ``author`` table.

  You can change the relationship names by heading to the ``Relationships`` tab of the concerned table and
  dropping and recreating the relationship with a name of your choice.

