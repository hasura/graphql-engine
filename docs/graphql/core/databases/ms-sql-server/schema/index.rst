.. meta::
   :description: Manage GraphQL schema over MS SQL Server with Hasura
   :keywords: hasura, docs, ms sql server, schema

.. _ms_sql_server_schema:

MS SQL Server: Schema
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Hasura GraphQL engine automatically generates your GraphQL schema and resolvers based on your tables/views
in MS SQL Server. **You don't need to write a GraphQL schema or resolvers.** See
:ref:`How Hasura GraphQL engine works <how_it_works>` for more details.

The Hasura console gives you UI tools that speed up your data-modelling process, or working with your existing database.
The console also automatically generates migrations or metadata files that you can edit directly and check into your
version control.

The Hasura GraphQL engine lets you do anything you would usually do with MS SQL Server by giving you GraphQL over native
MS SQL Server constructs.

**See:**

.. toctree::
  :maxdepth: 1

  Tables basics <tables>
  Table relationships <table-relationships/index>
  Extend with views <views>
  Customise auto-generated fields <custom-field-names>
  Using an existing database <using-existing-database>

.. TODO: DB-COMPATIBILITY
  .. toctree::
    :maxdepth: 1

    Tables basics <tables>
    Table relationships <table-relationships/index>
    Remote relationships <remote-relationships/index>
    Extend with views <views>
    Extend with SQL functions <custom-functions>
    Default field values <default-values/index>
    Enum type fields <enums>
    Computed fields <computed-fields>
    Customise auto-generated fields <custom-field-names>
    Data validations <data-validations>
    Using an existing database <using-existing-database>
    Relay schema <relay-schema>
