.. meta::
   :description: Manage mutations with Hasura
   :keywords: hasura, docs, mutation

.. _mutations:

Mutations
=========

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

GraphQL mutations are used to modify data on the server (i.e. write, update or delete data).

Hasura GraphQL engine auto-generates mutations as part of the GraphQL schema from your Postgres schema model.

Data of all tables in the database tracked by the GraphQL engine can be modified over the GraphQL endpoint.
If you have a tracked table in your database, its insert/update/delete mutation fields are added as nested
fields under the  ``mutation_root`` root level type.

Types of mutation requests
--------------------------

The following types of mutation requests are possible:

.. toctree::
  :maxdepth: 1

  Insert <insert>
  Upsert <upsert>
  Update <update>
  Delete <delete>
  multiple-mutations











