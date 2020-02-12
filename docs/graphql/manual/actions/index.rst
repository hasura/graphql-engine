Actions
=======

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


Actions are user defined mutations with custom business logic. Actions can be added to Hasura to handle various use cases such as data validation, data enrichment and other complex business logic.

.. thumbnail:: ../../../img/graphql/manual/actions/actions-hl-arch.png
   :class: no-shadow
   :alt: Actions high level architecture

What is an action?
------------------

An action is a way to extend Hasura's mutation root with an entirely custom mutation. A custom mutation consists of the following parts:

1. ``Definition``: The definition of the mutation
2. ``Handler``: The logic to be run when the mutation is executed
3. ``Kind``: Sync or async

Definition
**********

The action definition consists of the following:

- ``Action Name``: The action will be available as a mutation in the GraphQL schema named as the action name
- ``Arguments``: Arguments are used to pass dynamic values along with the mutation.
- ``Response type``: The GraphQL type of the response that the mutation will return. Actions can only return object types.

For instance, consider this action definition:

.. code-block:: graphql

    extend type Mutation {
      userLogin(username: String!, password: String!): UserInfo
    }

In this definition, we are extending the mutation root with an action called ``userLogin``.

- ``userLogin`` is the action name
- ``username`` and ``password`` are the arguments that accept non-nullable string values.
- ``UserInfo`` is the response type of the action

**Custom Types**

An action must return an object type. This means, you will have to define your custom types like so:

.. code-block:: graphql

    type UserInfo {
      accessToken: String!
      userId: Int!
    }

Read more about :doc:`custom types<types/index>`.

Handler
*******

Once you define the types, you also have to specify the logic to run when the action mutation is executed. This can be done in an HTTP webhook, also called as action handler; it could be a REST endpoint or a serverless function.

Learn more about :doc:`writing an action handler<action-handlers>`.

Kind
****

Actions are of two kinds:

* Synchronous actions: In "sync" actions, the GraphQL client is not aware of the event driven flow and gets the appropriate response within the GraphQL mutation response fields.
* :doc:`Asynchronous actions<async-actions>`: Async actions return the response to the client before receiving a response from the handler but they allow the client to subscribe to the response.

How it works?
-------------

* Hasura receives the GraphQL mutation and converts this request into an event payload.
* The event is captured, persisted and then delivered to the handler with the appropriate retry/delivery guarantees.
* The event handler runs and returns a response that is captured as an event and again persisted to the event store.
* The response is returned to the client synchronously or asynchronously based on the kind.


Learn more
----------

.. toctree::
  :maxdepth: 1
  :titlesonly:

  Getting started <getting-started>
  Custom Types <types/index>
  Action handlers <action-handlers>
  Async actions <async-actions>
  Codegen <codegen/index>
