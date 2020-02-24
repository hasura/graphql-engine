Scalars
=======

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

A GraphQL object type has a name and fields, but at some point those fields
have to resolve to some concrete data. That's where the scalar types come
in: they represent the leaves of the query.

Inbuilt scalars
---------------

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


Custom Scalars
--------------

Hasura allows you to define custom scalars. For example, if you want to define
a scalar called ``Date``, you can define it like.

.. code-block:: graphql

    scalar Date

These scalars can be used as arguments of the mutation or as fields of object
types and input types.



[Reference](https://graphql.org/learn/schema/#scalar-types)