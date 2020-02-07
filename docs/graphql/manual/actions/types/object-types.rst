Object Types
============

The most basic components of a GraphQL schema are object types, which just represent a kind of object a GraphQL query can return, and what fields it has. In the GraphQL SDL, we might represent it like this:

.. code-block:: graphql

    type UserInfo {
      accessToken: String! 
      userId: Int!
    }

This is an object type called ``UserInfo`` that has two fields:
* ``accessToken``: This field is of type ``String!`` (non-nullable ``String``)
* ``userId``: This field is of type ``Int!`` (non-nullable ``Int``)

[Reference](https://graphql.org/learn/schema/#object-types-and-fields)

Limitation
----------

Hasura does not allow a field of an object type to be an object type i.e. the fields of an object type could be only ``Scalars`` and ``Enums``.






