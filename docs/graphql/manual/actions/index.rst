.. meta::
   :description: Hasura actions
   :keywords: hasura, docs, actions

.. _actions:

Actions
=======

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

What are actions?
-----------------

Actions are a way to extend Hasura's schema with custom business
logic using custom queries and mutations. Actions can be
added to Hasura to handle various use cases such as data validation, data
enrichment from external sources and any other complex business logic.

.. thumbnail:: /img/graphql/manual/actions/actions-arch.png
   :class: no-shadow
   :alt: Actions high level architecture

.. admonition:: Support

   Actions are supported in Hasura GraphQL engine versions ``v.1.2.0`` and above.

   Actions are supported for **Postgres versions 10 and above**.

Action description
------------------

An action consists of the following parts:

1. ``Type``: The type of the action (``query`` or ``mutation``)
2. ``Definition``: The definition of the query or mutation
3. ``Handler``: The logic to be run when the query or mutation is executed
4. ``Kind``: Sync or async. In case of a query action, there is no ``Kind``
   associated with it. A query action is always sync

Type
****

Actions can be of two types:

- **Query action**: An action of type ``query`` extends the query root of the
  Hasura schema. This means that you can execute this action through a GraphQL query.
  Query actions must be used where you want to fetch data from a data source without
  changing anything on the data source.
- **Mutation action**: An action of type ``mutation`` extends the mutation root of
  the Hasura schema. This means that you can execute this action through a GraphQL
  mutation. Mutation actions must be used when you want to mutate the state of a data
  source and fetch some data.

Definition
**********

The action definition consists of the following:

- ``Action Name``: The action will be available as a query or mutation in the GraphQL
  schema named as the action name
- ``Arguments``: Arguments are used to pass dynamic values along with the
  query/mutation.
- ``Response type``: The GraphQL type of the response that the query or mutation will
  return. Actions can only return object types.

For instance, consider this action definition:

.. code-block:: graphql

    extend type Mutation {
      userLogin(username: String!, password: String!): UserInfo
    }

In this definition, we are extending the mutation root with an action called
``userLogin``.

- ``userLogin`` is the action name
- ``username`` and ``password`` are the arguments that accept non-nullable
  string values.
- ``UserInfo`` is the response type of the action

**Custom Types**

An action must return an object type. This means, you will have to define your
custom types like so:

.. code-block:: graphql

    type UserInfo {
      accessToken: String!
      userId: Int!
    }

Read more about :ref:`custom types<custom_types>`.

Handler
*******

Once you define the action types, you also have to specify the logic to run
when the action is executed. This can be done in an HTTP webhook,
also called the action handler. It could be a REST endpoint or a serverless
function.

Learn more about :ref:`writing an action handler<action_handlers>`.

Kind
****

**Mutation actions** are of two kinds:

* **Synchronous**: Sync actions return a response to the client after
  receiving a response from the handler.
* **Asynchronous**: :ref:`Async actions <async_actions>` return an
  ``action id`` as response to the client before receiving a response from the
  handler and allow the client to subscribe to the actual response using the
  ``action id``.

**Query actions** don't have a kind, they always behave like sync mutation actions.

How it works?
-------------

* Hasura receives the action GraphQL query or mutation and converts this request into an
  event payload.
* The event is captured, persisted and then delivered to the action handler with
  the appropriate retry/delivery guarantees.
* The action handler runs and returns a response that is captured as an event
  and again persisted to the event store.
* The action response is returned to the client synchronously or asynchronously
  based on the kind.

Actions vs. remote schemas
--------------------------

Both actions and remote schemas can be used to extend Hasura with business logic.
However, they have slightly different use cases.

**Actions**

Actions can be used when we want to call a REST endpoint from Hasura as a resolver for some custom types.
They are especially useful for setting up serverless functions as resolvers.

**Remote schemas**

If you have an existing GraphQL API or if you're comfortable building a GraphQL server yourself,
you can use :ref:`remote schemas <remote_schemas>` to add custom types and resolvers.

Learn more
----------

.. toctree::
  :maxdepth: 1
  :titlesonly:

  create
  Custom types <types/index>
  action-handlers
  async-actions
  Codegen <codegen/index>
  derive
  action-permissions
  reuse-types-actions
  debugging
  .. action-examples
