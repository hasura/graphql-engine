Subscriptions
=============

A GraphQL subscription is essentially a query where the client receives an event whenever the value of any field changes
upstream. The Hasura GraphQL engine supports subscriptions for all kind of queries. All the concepts of
:doc:`queries <../queries/index>` hold true with subscriptions as well.

You can turn any query into a subscription by simply replacing ``query`` with ``subscription`` as the operation type.

.. admonition:: Caveat

  The `GraphQL spec <http://facebook.github.io/graphql/June2018/#sec-Single-root-field>`_ allows for only one root
  field in a subscription. So though Hasura GraphQL engine supports multiple root fields in subscriptions, most clients
  will not allow it.

Some use cases
--------------

- :ref:`subscribe_field`
- :ref:`subscribe_table`
- :ref:`subscribe_derived`

Protocol
--------

Hasura GraphQL engine uses the `GraphQL over Websocket Protocol
<https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md>`_ by the
`apollographql/subscriptions-transport-ws <https://github.com/apollographql/subscriptions-transport-ws>`_ library
for sending and receiving events.

.. toctree::
  :maxdepth: 1
  :hidden:

  Sample use cases <use-cases>