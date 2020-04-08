.. meta::
   :description: Custom GraphQL types for Hasura actions
   :keywords: hasura, docs, actions, custom types

.. _custom_types:

Custom GraphQL types
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

You can add custom GraphQL types in Hasura that you can utilise for
defining your actions.


.. admonition:: Limitations

  It is currently not possible to define ``Interfaces`` and ``Union types``
  as custom types

Object types
------------

The most basic components of a GraphQL schema are object types,
which just represent a kind of object a GraphQL query can return, and what
fields it has. In the GraphQL SDL, we might represent it like this:

.. code-block:: graphql

    type UserInfo {
      accessToken: String!
      userId: Int!
    }

This is an object type called ``UserInfo`` that has two fields:

* ``accessToken``: This field is of type ``String!`` (non-nullable ``String``)
* ``userId``: This field is of type ``Int!`` (non-nullable ``Int``)

`See reference <https://graphql.org/learn/schema/#object-types-and-fields>`__

.. admonition:: Limitations

  Hasura does not allow a field of an object type to be another object type,
  i.e. the fields of an object type can only be ``scalars`` and ``enums``.

  For a more complicated structure, the object types can be connected to the rest
  of the graph via :ref:`relationships <custom_object_type_relationships>`.

.. _custom_object_type_relationships:

Relationships
*************

Custom object types can be connected to the rest of the graph by setting up
:ref:`relationships <relationships>` with tables/views.

**For example**, given the object type:

.. code-block:: graphql

  type UserInfo {
    accessToken: String!
    userId: Int!
  }

We can create an **object relationship** called ``createdUser`` between the
``UserInfo`` object type and the ``user`` table via the
``UserInfo::userId -> user::id`` fields.

The object type will now be modified as:

.. code-block:: graphql
  :emphasize-lines: 4

  type UserInfo {
    accessToken: String!
    userId: Int!
    createdUser: user
  }

Input types
-----------

You can pass complex objects as arguments to a mutation. This is particularly
valuable in the case of mutations where you might want to pass in a whole
object to be created. In the GraphQL SDL, input types look exactly the same as
regular object types, but with the keyword input instead of type:

.. code-block:: graphql

    input LoginInfo {
      username: String!
      password: String!
    }

A field of an input type could be a ``scalar``, an ``enum`` or another input type.

`See reference <https://graphql.org/learn/schema/#input-types>`__

Scalar types
------------

A GraphQL object type has a name and fields, but at some point those fields
have to resolve to some concrete data. That's where the scalar types come
in: they represent the leaves of the query.

Inbuilt scalars
***************

Hasura comes with some default GraphQL scalars that you can directly start using
while defining your actions:

* ``Int``: A signed 32‐bit integer.
* ``Float``: A signed double-precision floating-point value.
* ``String``: A UTF‐8 character sequence.
* ``Boolean``: true or false.
* ``ID``: The ID scalar type represents a unique identifier, often used to
  refetch an object or as the key for a cache. The ID type is serialized in
  the same way as a String; however, defining it as an ID signifies that it
  is not intended to be human‐readable.

`See reference <https://graphql.org/learn/schema/#scalar-types>`__

Custom scalars
**************

Hasura allows you to define custom scalars. For example, if you want to define
a scalar called ``Date``, you can define it like.

.. code-block:: graphql

    scalar Date

These scalars can be used as arguments of the mutation or as fields of object
types and input types.

.. admonition:: Postgres scalars

   The base types from Postgres can be reusable as custom scalars without explicitly declaring them.
   For example, the ``uuid`` type from ``pgcrypto`` extension can be used to resolve a UUID data.

Enum types
----------

Enums are a special kind of scalar that is restricted to a particular set of
allowed values. This allows you to:

* Validate that any arguments of this type are one of the allowed values
* Communicate through the type system that a field will always be one of a
  finite set of values

Here's what an enum definition might look like in the GraphQL schema language:

.. code-block:: graphql

    enum Color {
      RED
      GREEN
      BLUE
    }

This means that wherever we use the type ``Color`` in our schema, we expect it
to be exactly one of RED, GREEN, or BLUE.

`See reference <https://graphql.org/learn/schema/#enumeration-types>`__
