Schema
======

Hasura GraphQL engine automatically generates your GraphQL schema and resolvers based on your tables/views
in Postgres. **You don't need to write a GraphQL schema or resolvers.**

The Hasura console gives you UI tools that speed up your data-modeling process, or working with your existing database.
The console also automatically generates migrations or metadata files that you can edit directly and check into your
version control.

Hasura GraphQL engine lets you do anything you would usually do with Postgres by giving you GraphQL over native
Postgres constructs.

See:
^^^^

.. toctree::
  :maxdepth: 1

  Basics <basics>
  Relationships <relationships/index>
  Customise with views <views>
  Customise with schema stitching <schema-stitching>
  Adding custom resolvers <custom-resolvers>
  Enum type fields <enums>
  Default field values <default-values>
  Set values using SQL functions/stored procedures <sql-functions>
  Using an existing database <using-existing-database>
  Export GraphQL schema <export-graphql-schema>
  How schema generation works <how-it-works>
