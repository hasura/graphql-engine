.. meta::
   :description: Hasura actions
   :keywords: hasura, docs, actions

.. _actions:

Actions (beta)
==============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

What are actions?
-----------------

Actions are a way to extend Hasura's auto-generated mutations with entirely
custom mutations with custom business logic. Actions can be
added to Hasura to handle various use cases such as data validation, data
enrichment from external sources and any other complex business logic.

.. thumbnail:: ../../../img/graphql/manual/actions/actions-hl-arch.png
   :class: no-shadow
   :alt: Actions high level architecture

.. admonition:: Supported from

   Actions are currently available in beta in the pre-release versions of ``v1.2.0``.

   .. Actions are supported in versions ``v.1.2.0`` and above.

.. admonition:: Postgres support

   Actions are supported for ``Postgres versions 10 or higher``.

Action description
------------------

An action consists of the following parts:

1. ``Definition``: The definition of the mutation
2. ``Handler``: The logic to be run when the mutation is executed
3. ``Kind``: Sync or async

Definition
**********

The action definition consists of the following:

- ``Action Name``: The action will be available as a mutation in the GraphQL
  schema named as the action name
- ``Arguments``: Arguments are used to pass dynamic values along with the
  mutation.
- ``Response type``: The GraphQL type of the response that the mutation will
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
when the action mutation is executed. This can be done in an HTTP webhook,
also called the action handler. It could be a REST endpoint or a serverless
function.

Learn more about :ref:`writing an action handler<action_handlers>`.

Kind
****

Actions are of two kinds:

* **Synchronous actions**: Sync actions return a response to the client after
  receiving a response from the handler.
* **Asynchronous actions**: :ref:`Async actions <async_actions>` return an
  ``action id`` as response to the client before receiving a response from the
  handler and allow the client to subscribe to the actual response using the
  ``action id``.

How it works?
-------------

* Hasura receives the action GraphQL mutation and converts this request into an
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
  Codegen <codegen>
  derive
  action-connect
