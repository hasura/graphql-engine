.. meta::
   :description: Performance of Hasura GraphQL queries
   :keywords: hasura, docs, schema, queries, performance

.. _query_performance:

Query performance
=================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Sometimes, queries can be slow due to large data volume or levels of nesting. 
This page explains how to identify the query runtime, how the query plan caching in Hasura works, and how queries can be optimized.

.. _query_runtime:

Query runtime
-------------

In order to find out the execution time of a query, you can click on the ``Analyze`` button on the Hasura console:

.. thumbnail:: ../../../img/graphql/manual/queries/analyze-query.png
   :class: no-shadow
   :width: 75%
   :alt: Query analyze button on Hasura console

In the output of the query execution plan, the ``cost`` stands for execution time of the query, represented as a range of milliseconds.

.. thumbnail:: ../../../img/graphql/manual/queries/query-execution-plan.png
   :class: no-shadow
   :width: 75%
   :alt: Execution plan for Hasura GraphQL query

Query plan caching
------------------

A GraphQL query is processed in these steps:

1. The incoming GraphQL query is parsed into an AST which represents the GraphQL language.
2. The GraphQL AST is then validated against the schema to generate an internal representation.
3. The internal representation is then converted into a SQL statement (prepare it when possible).
4. The statement (or the prepared statement) is executed on Postgres to retrieve the result of the query.

In certain cases (most typical use cases), graphql-engine can construct a 'plan' for a query so that a new instance of the same query can be executed without the overhead of steps 1 to 3.

For example, let's consider the following query:



Optimize using variables
------------------------

Optimize using indexes
----------------------

`Postgres indexes <https://www.tutorialspoint.com/postgresql/postgresql_indexes.htm>`__ are special lookup tables that Hasura can use to speed up data lookup.
An index acts as a pointer to data in a table, and it works very similar to an index in the back of a book. 
If you look up the page of the data you want in the index first, you'll find it much quicker than searching the whole book.

An index can be added in the ``SQL -> Data`` tab in the Hasura console:

.. code-block:: plpgsql

  CREATE INDEX ifk_articles_author_id ON articles (author_id);

This statement sets an index on the ``author_id`` in the ``articles`` table.

Let's compare the new query runtime to :ref:`the one before adding the index <query_runtime>`:

.. thumbnail:: ../../../img/graphql/manual/queries/query-execution-plan-after-index.png
   :class: no-shadow
   :width: 75%
   :alt: Execution plan for Hasura GraphQL query

We can see that the query runtime has become almost 13 times faster by adding an index.
