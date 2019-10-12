“Compile” a GraphQL query to a single SQL query
===============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Part of Hasura is a transpiler that uses the metadata of mapping information for the data models to the GraphQL schema
to “compile” GraphQL queries to the SQL queries to fetch data from the database.

GraphQL query → GraphQL AST → SQL AST → SQL

.. thumbnail:: ../../../../img/graphql/manual/subscriptions/graphql-2-sql.png
  :alt: graphql to sql

This gets rid of the N+1 query problem and allows the database to optimise data-fetching now that it can see the entire
query but this in itself isn't enough as resolvers also enforce authorization rules by only fetching the data that is
allowed. We will therefore need to embed these authorization rules into the generated SQL.
