.. meta::
   :description: Use enums in Hasura
   :keywords: hasura, docs, schema, enum

.. _enums:

Enum type fields
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Enum type fields are restricted to a fixed set of allowed values.

Enums in a database
-------------------

In a relational database such as Postgres, an enum type field in a table can be defined in two ways:

.. _native_pg_enum:

Using `native Postgres enum types <https://www.postgresql.org/docs/current/datatype-enum.html>`__
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

While the most obvious solution, native enum types have significant drawbacks: they are not easily mutable.
New values cannot be added to an enum inside a transaction (that is, ``ALTER TYPE ... ADD VALUE`` is not
supported by transactional DDL), and values cannot be removed from an enum at all without completely dropping
and recreating it (which cannot be done if the enum is in use by *any* tables, views, or functions). Therefore,
native enum types should only be used for enums that are guaranteed to *never* change, such as days of the
week.

.. _reference_table_enum:

Using `foreign-key references <https://www.postgresql.org/docs/current/tutorial-fk.html>`__ to a single-column table
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This approach represents an enum using ordinary relational database concepts. The enum type is represented by a
table, and the values of the enum are rows in the table. Columns in other tables that use the enum are ordinary
foreign-key references to the enum table.

For enums with values that are dynamic and may require updates, such as a list of tags or user roles, this
approach is strongly recommended. Modifying an enum defined this way is easy: simply insert, update, or delete
rows in the enum table (and updates or deletes can even be cascaded to references, and they may be done within
a transaction).

Enums in the Hasura GraphQL engine
----------------------------------

Given the limitations of native Postgres enum types (as described :ref:`above <native_pg_enum>`), Hasura
currently only generates GraphQL enum types for enums defined using the
:ref:`referenced tables <reference_table_enum>` approach.

You may use native Postgres enum types in your database schema, but they will essentially be treated like text
fields in the generated GraphQL schema. Therefore, this guide focuses primarily on modeling an enum using a
reference table, but you may still use native Postgres enum types to help maintain data consistency in your
database. You can always choose to create a table with the values of a Postgres enum as shown in the
:ref:`section below <create_enum_table_from_pg_enum>`.

**Example:** Let’s say we have a database that tracks user information, and users may only have one of three specific
roles: user, moderator, or administrator. To represent that, we might have a ``users`` table with the following schema:

.. code-block:: sql

  CREATE TABLE users (
    id serial PRIMARY KEY,
    name text NOT NULL,
    role text NOT NULL
  );

Now we can insert some users into our database:

.. code-block:: sql

  INSERT INTO users (name, role) VALUES
    ('Alyssa', 'administrator'),
    ('Ben', 'moderator'),
    ('Gerald', 'user');

This works alright, but it doesn’t prevent us from inserting nonsensical values for ``role``, such as

.. code-block:: sql

  INSERT INTO users (name, role) VALUES
    ('Hal', 'spaghetti');

which we certainly don’t want. Hence we should create an enum to restrict the allowed values.

.. _create_enum_table:

Creating an enum compatible table
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To represent an enum, we’re going to create an _`enum table`, which for Hasura’s purposes is any table that meets
the following restrictions:

1. The table must have a single-column primary key of type ``text``. The values of this column are the legal values
   of the enum, and they must all be `valid GraphQL enum value names
   <https://graphql.github.io/graphql-spec/June2018/#EnumValue>`__.
2. Optionally, the table may have a second column, also of type ``text``, which will be used as a description of each
   value in the generated GraphQL schema.
3. The table must not contain any other columns.
4. The table must contain at least 1 row.

**For example**, to create an enum that represents our user roles, we would create the following table:

.. code-block:: sql

  CREATE TABLE user_role (
    value text PRIMARY KEY,
    comment text
  );

  INSERT INTO user_role (value, comment) VALUES
    ('user', 'Ordinary users'),
    ('moderator', 'Users with the privilege to ban users'),
    ('administrator', 'Users with the privilege to set users’ roles');

.. _create_enum_table_from_pg_enum:

.. admonition:: Creating an enum table from a native PG enum

  You can create a table containing the values of a PG enum by executing the following SQL:

  .. code-block:: sql

    CREATE TABLE "<my_enum_table>" (value TEXT PRIMARY KEY);
    INSERT INTO "<my_enum_table>" (value) (SELECT unnest(enum_range(NULL::"<my_enum>")))::text);

Next, we need to tell Hasura that this table represents an enum.

Setting a table as an enum table
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Once we have a table which satisfies the conditions for an enum table as described :ref:`above <create_enum_table>`,
we need to tell Hasura that this table represents an enum.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Head to the ``Modify`` tab of the table and toggle the switch in the
    ``Set table as enum`` section:

    .. thumbnail:: /img/graphql/manual/schema/enum-set.png
       :alt: Set table as enum

  .. tab:: Via CLI

    To set a table as an enum, change the ``tables.yaml`` file in the ``metadata`` directory as follows:

    .. code-block:: yaml
       :emphasize-lines: 4

        - table:
            schema: public
            name: user_role
          is_enum: true

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

    A table can be set as an enum via the following 2 methods:

    1. Passing ``true`` for the ``is_enum`` option of the :ref:`track_table metadata API <track_table>` while tracking a table:

    .. code-block:: http
      :emphasize-lines: 10

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "track_table",
        "args": {
          "schema": "public",
          "name": "user_role",
          "is_enum": true
        }
      }

    2. Using the :ref:`set_table_is_enum metadata API<set_table_is_enum>` to change whether or not an already-tracked table should be used as an enum:

    .. code-block:: http
      :emphasize-lines: 6-13

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "set_table_is_enum",
        "args": {
          "table": {
            "schema": "public",
            "name": "user_role"
          },
          "is_enum": true
        }
      }

Using an enum table
^^^^^^^^^^^^^^^^^^^

To set a field of a table as an enum in the GraphQL schema, we need to set a reference from it to the enum table
via a foreign key.

**For example**, to update our ``users`` table to reference the ``user_role`` enum table:

.. code-block:: sql

  ALTER TABLE users ADD CONSTRAINT
    users_role_fkey FOREIGN KEY (role) REFERENCES user_role;

Making queries using enum values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Once a table has been tracked as an enum, the GraphQL schema will be updated to expose the values of the
table as GraphQL enum values i.e. only the exposed values will be permitted for all fields referencing to it.

**For example**, the ``role`` column of the ``users`` table only permits the values in the ``user_role`` table:

.. code-block:: graphql

  type users {
    id: Int!
    name: String!
    role: user_role_enum!
  }

  enum user_role_enum {
    "Users with the privilege to set users’ roles"
    administrator

    "Users with the privilege to ban users"
    moderator

    "Ordinary users"
    user
  }

When making queries that filter on the ``role`` column, use the name of the enum value directly rather than providing
a string:

.. graphiql::
  :view_only:
  :query:
    {
      users(
        where: {
          role: {_eq: administrator}
        }
      ) {
        id
        name
      }
    }
  :response:
    {
      "data": {
        "users": [
          {
            "id": 1,
            "name": "Alyssa"
          }
        ]
      }
    }

Enums and migrations
^^^^^^^^^^^^^^^^^^^^

As enum tables have a requirement to contain at least 1 row, it is necessary to have a migration which inserts
values into an enum table. Otherwise while applying migrations an error would be thrown while trying to set the
table as an enum.

The migration which inserts values into an enum table needs to be between the migration creating the table
and the migration setting it as an enum.

This can be achieved via the console by performing the following steps while setting up an enum table:

1. Create the enum table
2. Use the ``RawSQL`` tab of the console to insert the enum values into the table and mark the insert as a migration
3. Set the table as an enum

You can also :ref:`manually create migration files <manual_migrations>` to achieve
this.

Current limitations
^^^^^^^^^^^^^^^^^^^

Currently, Hasura does not automatically detect changes to the contents of enum tables, so the GraphQL schema will
only be updated after :ref:`manually reloading metadata <reload_metadata_manual>` after inserting, updating, or deleting rows from an enum table.
