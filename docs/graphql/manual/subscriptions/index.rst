Subscriptions
=============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

A GraphQL subscription is essentially a query where the client receives an event whenever the value of any field changes
upstream. The Hasura GraphQL engine supports subscriptions for all kind of queries. All the concepts of
:doc:`queries <../queries/index>` hold true with subscriptions as well.

Convert a query to a subscription
---------------------------------

You can turn any query into a subscription by simply replacing ``query`` with ``subscription`` as the operation type.

.. admonition:: Caveat

  Hasura follows the `GraphQL spec <http://facebook.github.io/graphql/June2018/#sec-Single-root-field>`_ which 
  allows for only one root field in a subscription.

Use cases
---------

- :ref:`subscribe_field`
- :ref:`subscribe_table`
- :ref:`subscribe_derived`

Communication protocol
----------------------

Hasura GraphQL engine uses the `GraphQL over Websocket Protocol
<https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md>`_ by the
`apollographql/subscriptions-transport-ws <https://github.com/apollographql/subscriptions-transport-ws>`_ library
for sending and receiving events.

Cookie and Websockets
---------------------
Hasura GraphQL engine will read cookies sent by the browser when initiating a
websocket connection. Browser will send the cookie only if it is a secure cookie
(``secure`` flag in the cookie) and if the cookie has a ``HttpOnly`` flag.

Hasura will read this cookie and use it as headers when resolving authorization
(i.e. when resolving the auth webhook).

Cookies, Websockets and CORS
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
As browsers don't enforce Same Origin Policy (SOP) for Websockets, Hasura server
enforces the CORS rules when accepting the websocket connection.

It uses the provided CORS configuration (as per :ref:`configure-cors`).

1. When it is ``*``, the cookie is read the CORS check is not enforced.

2. When there are explicit domains, only if the request originates from one of
   the listed domains, the cookie will be read.

3. If CORS is disabled, the default behaviour is, the cookie won't be read
   (because of potential security issues). To override the behaviour, you can
   use the flag ``--ws-read-cookie`` or environment variable
   ``HASURA_GRAPHQL_WS_READ_COOKIE``. See
   :doc:`../deployment/graphql-engine-flags/reference` for the setting.


.. toctree::
  :maxdepth: 1
  :hidden:

  Sample use cases <use-cases>
