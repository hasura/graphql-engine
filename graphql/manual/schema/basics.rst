
Basics
======

Open the console
----------------

.. code:: bash

   hasura console

Create tables
-------------

Let's say you have 2 tables in Postgres.

First you need to tell Hasura to "track" those tables.

Try basic GraphQL queries
-------------------------
Hasura now generates a schema that allows you to perform queries and mutations on those tables.

Connect tables
--------------
If you now want to "connect" the 2 tables, and essentially create a "graph" on your underlying data models,
you can tell Hasura to create a relationship between them.

Once you do this, you can now query your graph with GraphQL 🤘
