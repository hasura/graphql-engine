.. meta::
   :description: Manage relationships between MS SQL Server tables/views in Hasura
   :keywords: hasura, docs, ms sql server, schema, relationship

.. _ms_sql_server_table_relationships:

MS SQL Server: Relationships between tables/views
=================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

To make :ref:`nested object queries <ms_sql_server_nested_object_queries>`, the tables/views in your database need to
be connected via relationships.

Let's say we have the following tables in our database: ``authors``, ``passport_infos``, ``articles`` and ``tags``.

Table relationships
-------------------

The tables/views in any relational database are typically related to each other via one of the
following types of table relationships:

+------------------+-------------------------------------+------------------------------------------------+
| Type             | Example                             | Meaning                                        |
+==================+=====================================+================================================+
| ``one-to-one``   | ``authors`` and ``passport_infos``  | - an ``author`` can have one ``passport_info`` |
|                  |                                     | - a ``passport_info`` can have one ``owner``   |
+------------------+-------------------------------------+------------------------------------------------+
| ``one-to-many``  | ``authors`` and ``articles``        | - an ``author`` can have many ``articles``     |
|                  |                                     | - an ``article`` can have one ``author``       |
+------------------+-------------------------------------+------------------------------------------------+
| ``many-to-many`` | ``articles`` and ``tags``           | - an ``article`` can have many ``tags``        |
|                  |                                     | - a ``tag`` can have many ``articles``         |
+------------------+-------------------------------------+------------------------------------------------+

.. _ms_sql_server_graphql_relationships:

GraphQL schema relationships
----------------------------

Each table relationship, as you can see from the above section, will have two component relationships
(one in either direction) in the GraphQL schema. These relationships can be one of the following types:

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

- :ref:`ms_sql_server_create_relationships`
- :ref:`ms_sql_server_rename_relationships`

Table relationships modelling guides
------------------------------------

The following guides will help you model the different types of table relationships in the database:

- :ref:`one_to_one_modelling`
- :ref:`one_to_many_modelling`
- :ref:`many_to_many_modelling`

.. toctree::
  :maxdepth: 1
  :hidden:

  Creating relationships <create>
  Renaming relationships <rename>
  Database modelling <database-modelling>
