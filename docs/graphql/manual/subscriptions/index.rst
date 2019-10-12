Subscriptions
=============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


What is a subscription?
---------------------------------

A GraphQL subscription is essentially a query where the client receives an event whenever the value of any field changes upstream. Hasura allows 'live queries' for clients (over GraphQL subscriptions). For example, a food ordering app can use a live query to show the live-status of an order for a particular user. The Hasura GraphQL engine supports subscriptions for all kinds of queries. All the concepts of :doc:`queries <../queries/index>` hold true for subscriptions as well. 

How do subscriptions work?
---------------------------------

If we think about our food ordering app example, for an app screen showing a “order status” for the current user for their order, a GraphQL query would fetch the latest order status and the location of the agent delivering the order. Underneath it, this query is sent as a string to a webserver that parses it, applies authorization rules and makes appropriate calls to things like databases to fetch the data for the app. It sends this data, in the exact shape that was requested, as a JSON.

Enter live-queries: Live queries is the idea of being able to subscribe to the latest result of a particular query. As the underlying data changes, the server should push the latest result to the client. 

For more details on Hasura's architecture and how subscriptions work, please `see here <https://github.com/hasura/graphql-engine/blob/master/architecture/live-queries.md>`_ . 

Convert a query to a subscription
---------------------------------

You can turn any query into a subscription by simply replacing ``query`` with ``subscription`` as the operation type.

.. admonition:: Caveat

  Hasura follows the `GraphQL spec <https://graphql.github.io/graphql-spec/June2018/#sec-Single-root-field>`_ which
  allows for only one root field in a subscription.

Use cases
---------

- :ref:`subscribe_field`
- :ref:`subscribe_table`
- :ref:`subscribe_derived`

Communication protocol
----------------------

Hasura GraphQL engine uses the `GraphQL over WebSocket Protocol
<https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md>`_ by the
`apollographql/subscriptions-transport-ws <https://github.com/apollographql/subscriptions-transport-ws>`_ library
for sending and receiving events.

.. admonition:: Setting headers for subscriptions with Apollo client

  If you are using Apollo Client, headers can be passed to a subscription by setting ``connectionParams`` while
  `creating the wsLink <https://www.apollographql.com/docs/react/data/subscriptions/#client-setup>`_:

  .. code-block:: js
    :emphasize-lines: 6-8

    // Create a WebSocket link:
    const wsLink = new WebSocketLink({
      uri: `<graphql-endpoint>`,
      options: {
        reconnect: true,
        connectionParams: {
          headers: {headers-object}
        }
      }
    });

  See `this <https://www.apollographql.com/docs/react/data/subscriptions/#authentication-over-websocket>`_ for more info on
  using ``connectionParams``.


Cookies and WebSockets
----------------------
The Hasura GraphQL engine will read cookies sent by the browser when initiating a
WebSocket connection. The browser will send the cookie only if it is a secure cookie
(``secure`` flag in the cookie) and if the cookie has a ``HttpOnly`` flag.

Hasura will read this cookie and use it as headers when resolving authorization
(i.e. when resolving the auth webhook).

Cookies, WebSockets and CORS
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
As browsers don't enforce Same Origin Policy (SOP) for websockets, the Hasura server
enforces the CORS rules when accepting the websocket connection.

It uses the provided CORS configuration (as per :ref:`configure-cors`).

1. When it is ``*``, the cookie is read and the CORS check is not enforced.

2. When there are explicit domains, the cookie will only be read if the request originates from one of
   the listed domains.

3. If CORS is disabled, the default behaviour is that the cookie won't be read
   (because of potential security issues). To override the behaviour, you can
   use the flag ``--ws-read-cookie`` or the environment variable
   ``HASURA_GRAPHQL_WS_READ_COOKIE``. See
   :doc:`../deployment/graphql-engine-flags/reference` for the setting.

.. toctree::
  :maxdepth: 1
  :hidden:

  Sample use cases <use-cases> 
