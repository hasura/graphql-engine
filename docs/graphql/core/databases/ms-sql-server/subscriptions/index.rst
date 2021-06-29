.. meta::
   :description: Manage subscriptions on MS SQL Server with Hasura
   :keywords: hasura, docs, ms sql server, subscription

.. _ms_sql_server_subscriptions:

MS SQL Server: Subscriptions
============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

A GraphQL subscription is essentially a query where the client receives an update whenever the value of any field
changes upstream.

Subscriptions are supported for all kinds of queries. All the concepts of
:ref:`queries <ms_sql_server_queries>` hold true for subscriptions as well.

Implementation
--------------

The Hasura GraphQL engine subscriptions are actually **live queries**, i.e. a subscription will return the
latest result of the query being made and not necessarily all the individual events leading up to the result.
By default, updates are delivered to clients every **1 sec**.


.. TODO: DB COMPATIBILITY
  See more details on :ref:`subscriptions execution and performance <ms_sql_server_subscriptions_execution_and_performance>`.

Convert a query to a subscription
---------------------------------

You can turn any query into a subscription by simply replacing ``query`` with ``subscription`` as the operation type.

.. admonition:: Caveat

  Hasura follows the `GraphQL spec <https://graphql.github.io/graphql-spec/June2018/#sec-Single-root-field>`__ which
  allows for only one root field in a subscription.

Use cases
---------

- :ref:`ms_sql_server_subscribe_field`
- :ref:`ms_sql_server_subscribe_table`
- :ref:`ms_sql_server_subscribe_derived`

Communication protocol
----------------------

Hasura GraphQL engine uses the `GraphQL over WebSocket Protocol
<https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md>`__ by the
`apollographql/subscriptions-transport-ws <https://github.com/apollographql/subscriptions-transport-ws>`__ library
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

  See `this <https://www.apollographql.com/docs/react/data/subscriptions/#authentication-over-websocket>`__ for more
  info on using ``connectionParams``.


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
   :ref:`server_flag_reference` for the setting.

.. toctree::
  :maxdepth: 1
  :hidden:

  Sample use cases <use-cases>

..   Execution and performance <execution-and-performance>
