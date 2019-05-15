Setting up GraphQL subscriptions using apollo-client
====================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Setup
-----
This guide assumes you have the basic apollo-client working as per
https://www.apollographql.com/docs/react/essentials/get-started.html and now you want to enable subscriptions.

Install packages
^^^^^^^^^^^^^^^^

.. code-block:: bash

  npm install --save apollo-client apollo-link-ws apollo-link-http apollo-link apollo-utilities apollo-cache-inmemory subscriptions-transport-ws

Once these packages are installed, import them as follows in the file where you have currently initialised your client
(usually your ``App.js`` file).

.. code-block:: js

  // Remove the apollo-boost import and change to this:
  import ApolloClient from "apollo-client";
  // Setup the network "links"
  import { WebSocketLink } from 'apollo-link-ws';
  import { HttpLink } from 'apollo-link-http';
  import { split } from 'apollo-link';
  import { getMainDefinition } from 'apollo-utilities';
  import { InMemoryCache } from 'apollo-cache-inmemory';


below these imports initialise your client to fetch subscriptions along with query and mutation.

.. code-block:: js

  const httpLink = new HttpLink({
    uri: "http://<your-app>/v1/graphql", // use https for secure endpoint
  });

  // Create a WebSocket link:
  const wsLink = new WebSocketLink({
    uri: "ws://<your-app>/v1/graphql", // use wss for a secure endpoint
    options: {
      reconnect: true
    }
  });

  // using the ability to split links, you can send data to each link
  // depending on what kind of operation is being sent
  const link = split(
    // split based on operation type
    ({ query }) => {
      const { kind, operation } = getMainDefinition(query);
      return kind === 'OperationDefinition' && operation === 'subscription';
    },
    wsLink,
    httpLink,
  );

  // Instantiate client
  const client = new ApolloClient({
    link,
    cache: new InMemoryCache()
  })

Write your subscription
-----------------------

Most likely, when switching from ``<Query>`` to ``<subscription>`` there are few things that you might miss out on.

Below is an example where you can convert a query to subscription and see what changes need to be made when doing so.


.. code-block:: js


  <Query
    query = {
      gql`
        query fetchTodos($user_id: Int!) {
            todos (
              where: { user_id: {_eq: $user_id }, is_completed: { _eq: false }},
              order_by: id_desc
            ) {
              id
              data
              is_completed
              created_at
              updated_at
            }
         }
      `
    }
  >

  //when written as subscription

  <subscription
    subscription = {
      gql`
        subscription fetchTodos($user_id: Int!) {
            todos (
              where: { user_id: {_eq: $user_id }, is_completed: { _eq: false }},
              order_by: id_desc
            ) {
              id
              data
              is_completed
              created_at
              updated_at
            }
         }
      `
    }
  >

We can see that there are in total 3 places where the word ``query`` is changed to ``subscription`` and has to be taken
care of when switching to subscriptions.

.. code-block:: none

  <Query                                    <subscription
    query = {gql`          ->                 subscription = {gql`
        query {                                 subscription {
          ...                                     ...

.. admonition:: Caveat

  If all the 3 changes are not made, **it works like a query instead of a subscription**
  since, the code that sets up apollo-link doesn't work.

  .. code-block:: js

    const link = split(
      // split based on operation type
      ({ query }) => {
        const { kind, operation } = getMainDefinition(query);
        console.log({ query: query, kind: kind, operation: operation });
        return kind === 'OperationDefinition' && operation === 'subscription';
      },
      wsLink,
      httpLink,
    )

Related Blog
------------

- `moving-from-apollo-boost-to-graphql-subscriptions-with-apollo-client <https://blog.hasura.io/moving-from-apollo-boost-to-graphql-subscriptions-with-apollo-client-cc0373e0adb0>`__.
