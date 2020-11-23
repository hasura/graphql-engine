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
part of the upstream remote schema for each role and can also be used to set preset
values for input values. By default, the ``admin`` role has unrestricted access to
the upstream remote schema.

.. _add_remote_schema_permissions:

add_remote_schema_permissions
-----------------------------

A remote schema permission is used to enforce constraints on the queries
to the remote schema. This is done by specifying the schema document
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
2. Add a preset value to the ``user_id`` argument of the ``user`` field defined in the ``Query`` object.
   We want the value of the preset to come from a session variable called ``x-hasura-user-id``.
3. Allow filtering of the messages only by ``name`` in the ``where`` argument
   of the ``user_messages`` field.
4. Allow only the fields ``hello``, ``messages`` and the ``user`` top level node in the ``Query`` object.
5. Expose only the ``query_root`` and not allow mutations of the role.

The schema document, implementing the above restrictions will look like:

.. code-block:: graphql

  type User {
    user_id: Int
    name: String
    userMessages(where: MessageWhereInpObj, includes: IncludeInpObj): [Message]
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
    user(user_id: Int! @preset(value: "x-hasura-user-id")): User
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
               "schema" : "type User {     user_id: Int     name: String     userMessages(where: MessageWhereInpObj, includes: IncludeInpObj): [Message]   }    interface Communication {     id: Int!     msg: String!   }    type Message implements Communication {     id: Int!     name: String!     msg: String!     errorMsg: String   }    input MessageWhereInpObj {     name: StringCompareObj   }    input IntCompareObj {     eq : Int     gt : Int     lt : Int   }    input StringCompareObj {     eq : String   }    input IncludeInpObj {     id: [Int]     name: [String]   }    type Query {     hello: String     messages(where: MessageWhereInpObj, includes: IncludeInpObj): [Message]     user(user_id: Int! @preset(value: \"x-hasura-user-id\")): User   }    schema {     query: Query  }"
           },
          "comment":"remote schema permissions for role: user"
       }
   }

Arguments Presets
^^^^^^^^^^^^^^^^^

Argument presets are useful when we need to have the values automatically set instead
of the user being able to explicitly specify the value for the field. Argument presets
are of two types:

1. Static Value
2. :ref:`Session Variable Value <dynamic_session_variables>`

A preset value can be added to an input value via the ``preset`` directive.

.. code-block:: graphql

   type User {
     name String
     id   Int
   }

   type Query {
     user(user_id: Int! @preset(value: 1))
   }

When an input field has a preset defined, it will be removed from the schema. So, following
the above example, the user won't be able to specify the ``user_id`` argument while querying
the ``user`` field and whenever the role executes the ``user`` field, the preset value will
get added by the graphql-engine before querying the remote schema.

A preset value can also be a dynamic session variable value. When the preset value is a
dynamic session variable value, then its value is read from the specified session variable
value when the query is executed and then its value is substituted in the appropriate place.

.. note::
   By default, if the input value preset contains a :ref:`session variable value <dynamic_session_variables>`,
   then its value will be resolved when the query is executed. To treat the session
   variable value as a literal value (avoiding resolving of the session variable
   value) can be done by specifying ``static`` as ``true`` while defining the preset.

   For example:

   .. code-block:: graphql

      type Query {
        hello(text: String! @preset(value: "x-hasura-hello", static: true))
      }

   In this case, ``"x-hasura-hello"`` will be the argument to the ``hello`` field
   whenever it's queried.

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
     - The remote schema permission definition
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

.. note::
   ``add_remote_schema_permissions`` will only work when the graphql-engine has enabled remote
   schema permissions. Remote schema permissions can be enabled by running the graphql-engine
   with the ``--enable-remote-schema-permissions`` server flag or by setting the   ``HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS`` environment variable.

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
