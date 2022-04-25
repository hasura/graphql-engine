.. meta::
   :description: Manage subscriptions on Postgres with Hasura
   :keywords: hasura, docs, postgres, subscription

.. _pg_subscriptions:

Postgres: Subscriptions
=======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

A GraphQL subscription is essentially a query where the client receives an update whenever the value of any field
changes upstream.

Subscriptions are supported for all kinds of queries. All the concepts of
:ref:`queries <pg_queries>` hold true for subscriptions as well.

.. admonition:: Caveat

  Hasura follows the `GraphQL spec <https://graphql.github.io/graphql-spec/June2018/#sec-Single-root-field>`__ which
  allows for only one root field in a subscription.

Types of subscriptions
----------------------

The Hasura GraphQL engine supports two kind of subscriptions:

Live queries
^^^^^^^^^^^^

A live query subscription will return the latest result of the query being made
and not necessarily all the individual events leading up to the result.
By default, updates are delivered to clients every **1 sec**.

See more details :ref:`here <pg_live_query_subscriptions>`.

Streaming subscriptions
^^^^^^^^^^^^^^^^^^^^^^^

A streaming subscription streams the response according to the cursor input
by the user. A streaming subscription is different from a live query as it sends individual rows
at a time and not the entire result set.

See more details :ref:`here <pg_streaming_subscriptions>`

Live query vs Streaming subscriptions
-------------------------------------

Suppose we need to display the messages of a group chat on a page, this can be done either via
live queries or streaming subscriptions. Let's see how they can be used and how they differ from each other.

1. Using **live query**

   With live query, we'll make the following query:

   .. code-block:: graphql

      subscription {
        messages (
          where: {group_id: 1},
          order_by: {created_at: asc}
        ) {
          id
          sender
          reciever
          content
          created_at
          edited_at
        }
      }

   The initial response for this subscription will be all the messages of the group. Let's say the initial
   response contained 100 messages. Now, if there is one more message sent to the group, then all 101 messages
   will be sent in a new response.


2. Using **streaming subscriptions**

   With streaming subscriptions, we'll make the following query:

   .. code-block:: graphql

      subscription {
        messages_stream (
          where: {group_id: 1},
          cursor: {initial_value: {created_at: now}},
          batch_size: 10
        ) {
          id
          sender
          reciever
          content
          created_at
          edited_at
        }
      }

   Here, we'll start getting all messages of the group in batches given by ``batch_size`` with ``created_at``
   greater than ``now``.

   Following the example of the live query, if we have 100 messages corresponding to the group and only
   5 messages with ``created_at`` greater than the current value of the cursor maintained by the cursor, then
   we will get only the 5 messages.

Communication protocol
----------------------

Hasura GraphQL engine uses the `GraphQL over WebSocket Protocol
<https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md>`__ by the
`apollographql/subscriptions-transport-ws <https://github.com/apollographql/subscriptions-transport-ws>`__ library and the
`GraphQL over WebSocket Protocol <https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md>`__
by the `graphql-ws <https://github.com/enisdenjo/graphql-ws>`__ library for sending and receiving events.
The GraphQL engine uses the ``Sec-WebSocket-Protocol`` header to determine
the server Implementation that'll be used. By default, the GraphQL engine will use the ``apollographql/subscriptions-transport-ws`` implementation.

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

  Live queries <livequery/index>
  Streaming subscriptions <streaming/index>
