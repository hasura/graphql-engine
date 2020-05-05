.. meta::
   :description: Troubleshoot Hasura GraphQL engine errors
   :keywords: hasura, docs, error, troubleshooting

.. _troubleshooting:

Troubleshooting Hasura GraphQL engine errors
============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Hasura GraphQL engine may not work as expected and will throw unexpected errors if the tables/views tracked by
the GraphQL engine are altered using ``psql`` or any other PostgreSQL client.

The Hasura GraphQL engine creates and maintains an **internal state** based on the database it is configured to use.
This internal state will comprise information about the tables/views, relationships and access control rules
defined on them using the Hasura GraphQL engine. See :ref:`Hasura GraphQL metadata schema <hasura_metadata_schema>`
for information on how this internal state is maintained. It is highly recommended doing any modifications to the
database schema only through the Hasura console to avoid corrupting the GraphQL engine's state.

Following are the list of error messages returned by the GraphQL engine when it encounters an inconsistent state:

Error: no such table/view exists in postgres
--------------------------------------------

This error is thrown when a table/view tracked by the Hasura GraphQL engine is not available in the
database.

For example, you will encounter the above error if you have:

- Created/tracked a table called ``author`` from console.
- Opened ``psql`` or ``adminer`` or any other PostgreSQL client and deleted ``author`` table.
- Restarted GraphQL engine.

In this example, the GraphQL engine expects the table ``author`` to be available in the database to
function properly but it can't find it.

Solution
^^^^^^^^

- Connect to the database and switch to ``hdb_catalog`` schema.
- Delete the row from ``hdb_table`` table where the column ``table_name`` has the value ``author``.
- Restart the GraphQL engine to verify.

Error: no foreign constraint exists on the given column
-------------------------------------------------------

The Hasura GraphQL engine validates all the relationships (created using foreign key/manually) before it starts serving.
When it encounters a relationship defined from table ``A -> B`` it looks for a foreign key constraint in table ``A``
and when it can't find it, it throws the above error.

Solution
^^^^^^^^

- Connect to the database and switch to ``hdb_catalog`` schema.
- In the ``hdb_relationship`` table, find the entry for the above relationship and delete it.
- Restart GraphQL engine to verify.

Error: field already exists
---------------------------

When a relationship is created using the Hasura GraphQL engine, it creates a special field with the relationship name
which is used while fetching nested objects using GraphQL.

Let's say we have tables called ``article`` and ``author`` as follows:

.. thumbnail:: /img/graphql/manual/troubleshooting/author_article.jpg
  :alt: article author schema 

Using the console if you have created a relationship named ``author`` from the ``article`` table to
the ``author`` table, the Hasura GraphQL engine will create a special field ``author`` in the ``article`` table in its
internal state. This field will be available via the GraphQL interface.

When this table is described using ``psql``, the ``author`` field will not be available as part of the list of fields
returned by the describe command as it is something added by Hasura GraphQL engine. Now if a new column is created
with the same name, i.e. ``author``, via ``psql``, the Hasura GraphQL engine will throw the above error when restarted as it has two
references to the ``author`` field for the ``article`` table.

Solution
^^^^^^^^

- Delete the problematic column from the table.
- Restart GraphQL engine to verify.

OR

- Connect to the database and switch to the ``hdb_catalog`` schema.
- In the ``hdb_relationship`` table, find the entry for the above relationship and delete it.
- Restart the GraphQL engine to verify.

Error: column does not exist
----------------------------

This error is thrown when a column of a table used by the Hasura GraphQL engine is not available in the
database.

For example, you will encounter the above error if you have:

- Created a permission rule using a column in a check.
- Opened ``psql`` or ``adminer`` or any other PostgreSQL client and deleted the column from the table.
- Restarted GraphQL engine.

In this example, the GraphQL engine expects the column to be available in the table to
function properly but it can't find it.

Solution
^^^^^^^^

- Connect to the database and switch to the ``hdb_catalog`` schema.
- Delete the row from the ``hdb_permission`` table where the column ``table_name`` has the same value as the table
  mentioned in the error and the column ``perm_def`` involves the missing column.
- Restart the GraphQL engine to verify.

Error: cannot continue due to new inconsistent metadata
-------------------------------------------------------

Some updates to the Hasura GraphQL engine may have :ref:`Hasura catalogue <hasura_metadata_schema>` version bumps. The GraphQL engine server
automatically migrates the catalogue to the latest version on startup. This migration may fail if the previous metadata state is inconsistent.

Solution
^^^^^^^^

- Start the older version of the GraphQL engine.
- Open the Hasura console to find the inconsistencies.
- Clear the inconsistencies.
- Start the newer version.
