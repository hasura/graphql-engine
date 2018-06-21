GraphQL over an existing database
=================================

When you have an existing database and you manage your Postgres schema through your existing migrations tooling, you don't need to
create tables or views or run DDL through the Hasura console.

All you need to do is indicate to Hasura, which tables and views you want to expose over GraphQL and how they are connected to
each other so that you can query them as a "graph".


Track tables
------------

.. note::

   TODO

Track foreign-keys
------------------

.. note::

   TODO

Create new relationships
------------------------

.. note::

   TODO
