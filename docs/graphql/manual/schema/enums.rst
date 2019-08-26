Enum type fields
================

Enum type fields are restricted to a fixed set of allowed values. In a relational database such as
Postgres, an enum type field in a table can be defined in two ways:

1. Using `native Postgres enum types <https://www.postgresql.org/docs/current/datatype-enum.html>`__.

   While the most obvious solution, native enum types have significant drawbacks: they are not easily mutable.
   New values cannot be added to an enum inside a transaction (that is, ``ALTER TYPE ... ADD VALUE`` is not
   supported by transactional DDL), and values cannot be removed from an enum at all without completely dropping
   and recreating it (which cannot be done if the enum is in use by *any* tables, views, or functions). Therefore,
   native enum types should only be used for enums that are guaranteed to *never* change, such as days of the
   week.

2. Using `foreign-key references <https://www.postgresql.org/docs/current/tutorial-fk.html>`__ to a single-column
   table.

   This approach represents an enum using ordinary relational database concepts. The enum type is represented by a
   table, and the values of the enum are rows in the table. Columns in other tables that use the enum are ordinary
   foreign-key references to the enum table.

   For enums with values that are dynamic and may require updates, such as a list of tags or user roles, this
   approach is strongly recommended. Modifying an enum defined this way is easy: simply insert, update, or delete
   rows in the enum table (and updates or deletes can even be cascaded to references, and they may be done within
   a transaction).

Given the limitations of native Postgres enum types, Hasura currently only generates GraphQL enum types for enums
defined using the second approach (i.e. referenced tables). You may use native Postgres enum types in your database
schema, but they will essentially be treated like text fields in the generated GraphQL schema. Therefore, this guide
focuses primarily on modeling an enum using a reference table, but you may still use native Postgres enum types to
help maintain data consistency in your database.

Example: Modeling an enum using an enum table
---------------------------------------------

Let’s say we have a database that tracks user information, and users may only have one of three specific roles: user,
moderator, or administrator. To represent that, we might have a ``users`` table with the following schema:

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

which we certainly don’t want. Let’s create an enum to restrict the allowed values.

Create an enum table
^^^^^^^^^^^^^^^^^^^^

To represent our enum, we’re going to create an _`enum table`, which for Hasura’s purposes is any table that meets
the following restrictions:

1. The table must have a single-column primary key of type ``text``. The values of this column are the legal values
   of the enum, and they must all be `valid GraphQL enum value names
   <https://graphql.github.io/graphql-spec/June2018/#EnumValue>`__.
2. Optionally, the table may have a second column, also of type ``text``, which will be used as a description of each
   value in the generated GraphQL schema.
3. The table may not contain any other columns.

For example, to create an enum that represents our user roles, we would create the following table:

.. code-block:: sql

  CREATE TABLE user_role (
    value text PRIMARY KEY,
    comment text
  );

  INSERT INTO user_role (value, comment) VALUES
    ('user', 'Ordinary users'),
    ('moderator', 'Users with the privilege to ban users'),
    ('administrator', 'Users with the privilege to set users’ roles');

Use the enum table
^^^^^^^^^^^^^^^^^^

Now that we’ve created an enum table, we need to update our ``users`` table to reference it:

.. code-block:: sql

  ALTER TABLE users ADD CONSTRAINT
    users_role_fkey FOREIGN KEY (role) REFERENCES user_role;

Next, we need to tell Hasura that this table represents an enum. We can do that by passing ``true`` for the
``is_enum`` option of the :ref:`track_table` API, or we can use the :ref:`set_table_is_enum` API to change whether or
not an already-tracked table should be used as an enum:

.. code-block:: http

  POST /v1/query HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
    "type": "track_table",
    "args": {
      "table": {
        "schema": "public",
        "name": "user_role"
      },
      "is_enum": true
    }
  }

Make queries using enum values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Once the table has been tracked as an enum, the GraphQL schema will be updated to reflect that the ``role`` column of
the ``users`` table only permits the values in the ``user_role`` table:

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
      users(where: {role: {_eq: administrator}}) {
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

.. admonition:: Current limitations

  Currently, Hasura does not automatically detect changes to the contents of enum tables, so the GraphQL schema will
  only be updated after manually reloading metadata after inserting, updating, or deleting rows from an enum table.
