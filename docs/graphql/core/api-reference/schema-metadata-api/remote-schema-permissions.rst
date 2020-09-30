.. meta::
   :description: Manage remote schema permissions with the Hasura metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, remote schema permissions, permission

.. _remote_schema_api_permission:

Schema/Metadata API Reference: Remote Schema Permissions
========================================================

.. contents:: Table of contents
   :backlinks: none
   :depth: 1
   :local:

Introduction
------------

The permission layer is designed to restrict the operations that can be
performed by various users. Permissions can be defined to only expose a certain
part of the upstram remote schema for each role. By default, the ``admin`` role
has unrestricted access to the upstream remote schema.

.. _add_remote_schema_permissions:

add_remote_schema_permissions
-----------------------------

A remote schema permission is used to enforce constraints on the query that's
being queried to the remote schema. This is done by specifying the schema document
(GraphQL language IDL) which should be a subset of the upstream remote schema.

Suppose the following is the schema document of the upstream remote.

.. code-block:: graphql

  type User {
    user_id: Int
    name: String
    phone: String
    userMessages(whered: MessageWhereInpObj, includes: IncludeInpObj): [Message]
  }

  interface Communication {
    id: Int!
    msg: String!
  }

  type Message implements Communication {
    id: Int!
    name: String!
    msg: String!
    errorMsg: String
  }

  input MessageWhereInpObj {
    id: IntCompareObj
    name: StringCompareObj
  }

  input IntCompareObj {
    eq : Int
    gt : Int
    lt : Int
  }

  input StringCompareObj {
    eq : String
  }

  input IncludeInpObj {
    id: [Int]
    name: [String]
  }

  type Query {
    hello: String
    messages(where: MessageWhereInpObj, includes: IncludeInpObj): [Message]
    user(user_id: Int!): User
    users(user_ids: [Int]!): [User]
    message(id: Int!) : Message
  }

  type mutation_root {
    insert_user: (name: String!, phone: String!): User
  }

  schema {
    query: Query
    mutation: mutation_root
  }

Let's say we want to impose some restrictions on the ``user`` role:

1. Expose only the ``user_id``, ``name`` and the ``user_messages`` field in the ``User`` object.
2. Allow filtering of the messages only by ``name`` in the ``whered`` argument
   of the ``user_messages`` field.
3. Allow only ``hello``, ``messages`` and the ``user`` top level node in the ``Query`` object.
4. Expose only the ``query_root``.

The schema document, implementing the above restrictions will look like:

.. code-block:: graphql

  type User {
    user_id: Int
    name: String
    userMessages(whered: MessageWhereInpObj, includes: IncludeInpObj): [Message]
  }

  interface Communication {
    id: Int!
    msg: String!
  }

  type Message implements Communication {
    id: Int!
    name: String!
    msg: String!
    errorMsg: String
  }

  input MessageWhereInpObj {
    name: StringCompareObj
  }

  input IntCompareObj {
    eq : Int
    gt : Int
    lt : Int
  }

  input StringCompareObj {
    eq : String
  }

  input IncludeInpObj {
    id: [Int]
    name: [String]
  }

  type Query {
    hello: String
    messages(where: MessageWhereInpObj, includes: IncludeInpObj): [Message]
    user(user_id: Int!): User
  }

  schema {
    query: Query
  }

To add the remote schema permission for the role ``user``, the following
API should be called with the schema document.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "add_remote_schema_permissions",
       "args" : {
           "remote_schema" : "user_messages",
           "role" : "user",
           "definition" : {
               "schema" : "type User {     user_id: Int     name: String     userMessages(whered: MessageWhereInpObj, includes: IncludeInpObj): [Message]   }    interface Communication {     id: Int!     msg: String!   }    type Message implements Communication {     id: Int!     name: String!     msg: String!     errorMsg: String   }    input MessageWhereInpObj {     name: StringCompareObj   }    input IntCompareObj {     eq : Int     gt : Int     lt : Int   }    input StringCompareObj {     eq : String   }    input IncludeInpObj {     id: [Int]     name: [String]   }    type Query {     hello: String     messages(where: MessageWhereInpObj, includes: IncludeInpObj): [Message]     user(user_id: Int!): User   }    schema {     query: Query  }"
           },
          "comment":"remote schema permissions for role: user"
       }
   }

.. _add_remote_schema_permissions_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - remote_schema
     - true
     - :ref:`RemoteSchemaName`
     - Name of the remote schema
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - definition
     - true
     - RemoteSchemaPermission_
     - The permission definition
   * - comment
     - false
     - text
     - Comment

.. _RemoteSchemaPermission:

RemoteSchemaPermission
&&&&&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - schema
     - true
     - GraphQL SDL
     - GraphQL SDL defining the role based schema

.. _drop_remote_schema_permissions:

drop_remote_schema_permissions
------------------------------

The ``drop_remote_schema_permissions`` API is used to drop an existing delete permission for a role on a remote schema.

An example:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "drop_remote_schema_permissions",
       "args" : {
           "remote_schema" : "user_messages",
           "role" : "user"
       }
   }

.. _drop_remote_schema_permissions_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`RemoteSchemaName`
     - Name of the remote schema
   * - role
     - true
     - :ref:`RoleName`
     - Role
