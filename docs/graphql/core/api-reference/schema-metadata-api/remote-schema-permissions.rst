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

Remote schema permissions can be defined to:

1. Expose only certain parts of the remote schema to a role
2. Preset arguments with static values or session variables for any field.

By default, the ``admin`` role has unrestricted access to
the  remote schema.

.. _add_remote_schema_permissions:

add_remote_schema_permissions
-----------------------------

This API takes the schema `(GraphQL IDL format) <http://spec.graphql.org/June2018/#sec-Type-System>`__
which should be a subset of the remote schema and the role for which this restricted schema is exposed.
The schema also accepts a custom ``@preset`` directive for setting argument presets.



Suppose the following is the schema document of the  remote.

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
5. Expose only the ``query_root`` and not allow mutations for the role.

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

Argument Presets
^^^^^^^^^^^^^^^^

Argument presets can be used to automatically inject input values for fields
during execution. This way the field is executed with limited input values. Argument
presets are of two types:

1. Static Value
2. :ref:`Session Variable <dynamic_session_variables>`

A preset value can be added to an input value via the ``@preset`` directive.

.. code-block:: graphql

   type User {
     name String
     id   Int
   }

   type Query {
     user(user_id: Int! @preset(value: 1))
   }

When an input field has a preset defined, it will be removed from the exposed schema. So, following
the above example, the user won't be able to specify the ``user_id`` argument while querying
the ``user`` field and whenever the role executes the ``user`` field, the preset value will
get added before querying the remote schema.

A preset value can also reference a session variable. When the preset value has a
session variable, then its value is resolved and then added before querying the remote schema.

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

Remote Relationship Permissions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Permissions for remote relationships are derived from the role's remote schema permissions.
When permissions for a given remote relationship cannot be derived from the remote schema
permissions of a given role, that remote relationship will not be accessible to that role.

Cases when the remote relationship cannot be derived are:
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

1. There are no remote schema permissions configured for the role for the remote join's remote schema.
2. The remote join field is not accessible to the role.
3. Any of the type (both output and input types) used in the remote join field is not accessible to the role.

When a remote field's argument contains a preset and the same argument
is used for creating a remote relationship, then the **remote presets will be
overridden by the remote join configuration**. For example:

Let's say we have a table called ``customer`` and we have a remote schema called
``payments`` and we have a remote relationship ``customer_transactions_history`` defined
which joins ``customer`` to ``transactions`` field of the ``payments`` field.

Suppose, the ``payments`` remote schema is defined in the following way:

.. code-block:: graphql

   type Transaction {
     customer_id    Int!
     amount         Int!
     time           String!
     merchant       String!
   }

   type Query {
     transactions(customer_id: String!, limit: Int): [Transaction]
   }

And, the ``customer`` table is defined in the following manner.

.. code-block:: sql

   CREATE TABLE customer (
     id SERIAL PRIMARY KEY,
     name TEXT NOT NULL
   );

The remote relationship is defined to join the ``id`` field from the
``customer`` table to the ``customer_id`` argument of the ``transactions``
field.

We only allow the ``user`` role to access the ``amount`` and ``time`` fiels of
the ``Transaction`` object, and introduce a preset for the ``limit`` argument
of the ``transaction`` field, resulting in the following schema being presented.

.. code-block:: graphql

   type Transaction {
     amount   Int!
     time     String!
   }

   type Query {
     transactions(customer_id: String!, limit: Int @preset(value: 10)): [Transaction]
   }

Two changes have been made for the ``user`` role:

1. The ``merchant`` and ``customer_id`` fields are not accessible in the ``Transaction`` object.
2. The ``limit`` argument has a preset of 10.

Now, consider the following query:

.. code-block:: graphql

   query {
     customer {
       name
       customer_transactions_history {
         amount
         time
       }
     }
   }

The ``user`` role won't be able to provide the value for the ``limit`` argument in
the ``customer_transactions_history`` field because the ``limit`` has a preset set
and the value will be added by the graphql-engine before it queries the remote schema.

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
""""""""""""""""""""""

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
