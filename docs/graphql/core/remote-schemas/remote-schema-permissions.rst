.. meta::
   :description: Remote schema permissions
   :keywords: authorization, docs, remote schema, permissions

.. _remote_schema_permissions:

Remote schema permissions
=========================

.. contents:: Table of contents
   :backlinks: none
   :depth: 1
   :local:

Introduction
------------

Hasura supports :ref:`role-based authorization <authorization>` for remote schemas.

Remote schema permissions can be defined to:

1. Expose only certain parts of the remote schema to a role
2. Preset arguments with static values or session variables for any field.

.. admonition:: Supported from

   Remote schema permissions are supported in Hasura GraphQL engine versions
   ``v1.4.0`` and above.

.. note::

   Remote schema permissions are **not** enabled by default in the graphql-engine.
   To enable them, you will have to run the graphql-engine either with the
   server flag ``--enable-remote-schema-permissions`` or environment variable
   ``HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS`` set to ``true``. When remote
   schema permissions are not enabled in the graphql-engine, the remote schemas are
   considered to be a public entity i.e. all roles will have unrestricted access to the
   remote schema.

.. note::

Role based remote schemas
-------------------------

Role based remote schemas allow you to expose only certain parts of the remote schema
. You can choose to remove any fields from objects, interfaces and input object types,
doing this will ensure that these fields are not exposed for the role and they will not
be able to query the fields that have been hidden.

For example, let's say we have the following remote schema added to the
graphql-engine:

.. code-block:: graphql

   type User {
     id         ID!
     first_name String!
     last_name  String!
     phone      String!
     email      String!
   }

   type Query {
     user(id: ID!) : User
     get_users_by_name (first_name: String!, last_name:String): [User]
   }

Now, we want to expose only certain fields of the ``User`` object for the
``public`` role here. The ``public`` role should not be allowed to access
the ``id``, ``email`` and ``phone`` fields of the ``User`` object. Now, since
the ``public`` role doesn't have access to the ``id`` field of the ``User`` object and
let's say that the ``id`` argument of the ``user`` field defined in the ``Query`` object
is the same as the ``id`` field of the ``User`` object, there will be no way of exposing the
``user`` field in the ``Query`` object, so we'll remove that field as well.

We can accomplish this by specifying the restricted schema (in GraphQL IDL format) for the
``public`` role. In the above case, it will be:

.. code-block:: graphql

   type User {
     first_name String!
     last_name  String!
   }

   type Query {
     get_users_by_name (first_name: String!, last_name: String): [User]
   }

We use the above schema document to configure the remote schema permissions for the ``public``
role by using the :ref:`add_remote_schema_permissions` API.

You can modify different `GraphQL Types <https://spec.graphql.org/June2018/#sec-Types>`__ in the following manner:

1. Scalar - A scalar definition cannot be modified differently from its correponding remote schema scalar definition.
2. Object - An object can omit some of the fields from its definition.
3. Interface - An interface, like the object type, can omit some of the fields from its definition.
4. Union - A union can be modified to only support a subset of the ``possibleTypes`` of its original union definition.
5. Enum - An enum can be configured to omit some enum values from its definition.
6. Input object - An input object, just like object type, can omit some of the (input) fields from its definition.

In a `field definition <https://spec.graphql.org/June2018/#FieldDefinition>`__ the arguments can
be configured to only expose a subset of the arguments defined.

For example, let's consider the remote schema used in the example above, but in this case we
want the ``public`` role to use the ``get_user_by_name`` with only the ``first_name``
argument and the ``public`` role should not be able to access the ``last_name`` argument.
The schema should look like:

.. code-block:: graphql

   type User {
     first_name String!
     last_name  String!
   }

   type Query {
     get_users_by_name (first_name: String!): [User]
   }

Argument presets
----------------

The role-based schema only helps in changing the type definitions that are exposed. Argument
presets are used to constrain the input values in fields.

Argument presets automatically inject values from session variables or static values during execution.
Arguments which are preset will not be exposed in the schema.
Argument presets are set on an argument value using the ``@preset`` directive.

.. note::

   A preset value can be defined only at the ``INPUT_FIELD_DEFINITION`` and ``ARGUMENT_DEFINITION``
   system directive locations i.e. only at an input object field or an argument field.

For example, let's say we have the following remote schema added to the
graphql-engine:

.. code-block:: graphql

   type User {
     id         ID!
     first_name String!
     last_name  String!
     phone      String!
     email      String!
   }

   type Activity {
     name          String!
     activity_type String!
     created_at    String!
   }

   type Query {
     get_user(id: ID!) : User
     get_user_activities(user_id: ID!, limit: Int!): [Activity]
   }

We want to configure the ``user`` role to only be able to query their
own record. To do this, we need to preset the ``id`` parameter of the ``get_user``
field defined in the ``Query`` object. Let's say we have the value of the ``id``
argument set in one of the :ref:`session variables <dynamic_session_variables>`, we can
preset the ``id`` argument with the session variable. Using the above schema,
we can do that in the following manner:

.. code-block:: graphql

   type Query {
     get_user(id: ID! @preset(value: "x-hasura-user-id")) : User
     get_user_activities(user_id: ID!, limit: Int!)
   }

Configuring the remote schema for the ``user`` role with the above schema
will remove the ``id`` argument from the schema and the value of the ``id``
argument will get injected via the ``x-hasura-user-id`` session variable, whenever the
``user`` role executes a query containing the ``get_user`` field.

Preset values can also be static values.

For example:

Suppose, we want the ``user`` role to allow to only get 10 of the user activities using the
``get_user_activities`` field, we can do that by setting a ``preset`` value for the
``limit`` argument of the ``get_user_activities`` to 10. The schema implementing
this change should look like:

.. code-block:: graphql

   type Query {
     get_user(id: ID! @preset(value: "x-hasura-user-id")) : User
     get_user_activities(user_id: ID!, limit: Int! @preset(value: 10)) : [Activity]
   }

.. note::

   By default, any preset string value in the format of  ``x-hasura-*`` is assumed
   to be a :ref:`session variable <dynamic_session_variables>`. To override this
   behaviour i.e. to treat the value literally, the ``static`` argument equal to ``true``
   needs to be added in the ``preset`` directive. In the following example,
   the ``x-hasura-user-id`` will be treated literally.

   .. code-block:: graphql

     get_user(id: ID! @preset(value: "x-hasura-user-id", static: true)) : User

Input object field presets
^^^^^^^^^^^^^^^^^^^^^^^^^^

Input object fields can also have preset values set. When an input object
contains multiple fields and only some of them have a preset set, the other
fields which don't contain a preset can be queried by the user and when
the query is executed, the user provided arguments are merged with the input
object field preset arguments.

Let's see an example, to see input object field presets in action.

Suppose, a remote schema with the following schema is added to the graphql-engine:

.. code-block:: graphql

   input MessageInput {
     from:       ID!
     to:         ID!
     content:    String!
   }

   type Message {
     from:    ID!
     to:      ID!
     content: String
   }

   type Query {
     get_user_messages(user_id: ID!): [Message]
   }

   type Mutation {
     create_message(message: MessageInput!): Bool
   }

We want to configure the remote schema in a way that when the ``user`` role
creates a new message (using ``create_message``), we want the value of the ``from`` field
of the ``MessageInput`` to come from the ``x-hasura-user-id`` session variable and the other
fields (``to`` and ``content``) to be set by the user. The schema for the ``user``
role should be configured in the following manner:

.. code-block:: graphql

   input MessageInput {
     from:       ID! @preset(value: "x-hasura-user-id")
     to:         ID!
     content:    String!
   }

   type Message {
     from:    ID!
     to:      ID!
     content: String
   }

   type Query {
     get_user_messages(user_id: ID!): [Message]
   }

   type Mutation {
     create_message(message: MessageInput!)
   }

Now, when the ``user`` role wants to create a new message, they can
do it in the following manner:

.. code-block:: graphql

   mutation {
     create_message(message: {to: "2", content: "hello world"})
   }

The ``from`` field will get injected into the input object before the
graphql-engine queries the remote server. The final query that will
be sent to the remote server will be:

.. code-block:: graphql

   mutation {
     create_message(message: {to: "2", content: "hello world", from: "<x-hasura-user-id>"})
   }
