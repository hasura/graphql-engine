.. meta::
   :description: Manage relationships between tables/views in Hasura
   :keywords: hasura, docs, schema, relationship

Relationships between tables/views
==================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

To make :doc:`nested object queries <../../queries/nested-object-queries>`, the tables/views in your database need to
be connected via relationships.

Let's say we have the following tables in our database: ``author``, ``passport_info``, ``article`` and ``tag``.

Table relationships
-------------------

The tables/views in any relational database are typically related to each other via one of the
following types of table relationships:

+------------------+-----------------------------------+------------------------------------------------+
| Type             | Example                           | Meaning                                        |
+==================+===================================+================================================+
| ``one-to-one``   | ``author`` and ``passport_info``  | - an ``author`` can have one ``passport_info`` |
|                  |                                   | - a ``passport_info`` can have one ``owner``   |
+------------------+-----------------------------------+------------------------------------------------+
| ``one-to-many``  | ``author`` and ``article``        | - an ``author`` can have many ``articles``     |
|                  |                                   | - an ``article`` can have one ``author``       |
+------------------+-----------------------------------+------------------------------------------------+
| ``many-to-many`` | ``article`` and ``tag``           | - an ``article`` can have many ``tags``        |
|                  |                                   | - a ``tag`` can have many ``articles``         |
+------------------+-----------------------------------+------------------------------------------------+

GraphQL schema relationships
----------------------------

As you can see, each table relationship will have two component relationships (one in either direction) in the GraphQL
schema. These relationships can be one of the following types:

+-----------------------------------------+------------------------------------------+---------------------------------------------------------------------------------------+
| Type                                    | Example                                  | Meaning                                                                               |
+=========================================+==========================================+=======================================================================================+
| ``object relationship`` (one-to-one)    | an ``article`` can have one ``author``   | an ``article`` object will have a single nested author object called ``author``       |
+-----------------------------------------+------------------------------------------+---------------------------------------------------------------------------------------+
| ``array relationship`` (one-to-many)    | an ``author`` can have many ``articles`` | an ``author`` object will have an array of nested article objects called ``articles`` |
+-----------------------------------------+------------------------------------------+---------------------------------------------------------------------------------------+

.. note::

  The relationship name is used to refer to the nested objects in queries. For example, "``articles``" of an ``author``
  and "``author``" of an ``article``.

Managing GraphQL relationships
------------------------------

See the following to manage the object/array relationships between tables/views for the GraphQL schema:

- :doc:`create`
- :doc:`rename`

Table relationships modelling guides
------------------------------------

The following guides will help you model the different types of table relationships in the database:

- :doc:`database-modelling/one-to-one`
- :doc:`database-modelling/one-to-many`
- :doc:`database-modelling/many-to-many`

.. toctree::
  :maxdepth: 1
  :hidden:

  create
  rename
  Database modelling <database-modelling/index>
