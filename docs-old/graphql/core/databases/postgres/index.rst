.. meta::
  :description: Hasura Postgres database support
  :keywords: hasura, docs, databases, postgres

.. _database_postgres:

Postgres
========

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Hasura allows connecting to a Postgres database and build an GraphQL API based on the database schema.

.. admonition:: Supported Postgres versions

  Hasura GraphQL engine supports **Postgres 9.5 and above**

Postgres flavours
-----------------

Hasura also supports databases with full Postgres compatibility like
Yugabyte, Timescale, Citus, Aurora, RDS.

We have more distributed flavours like CockroachDB coming soon. See `GitHub issue <https://github.com/hasura/graphql-engine/issues/678>`__.

Curious about any other Postgres flavours? Any other questions? Ask us on
`GitHub discussions <https://github.com/hasura/graphql-engine/discussions>`__


Know more
---------

.. toctree::
  :maxdepth: 1
  :titlesonly:

  Schema <schema/index>
  Queries <queries/index>
  Mutations <mutations/index>
  Subscriptions <subscriptions/index>
  Supported Postgres types <postgresql-types>
