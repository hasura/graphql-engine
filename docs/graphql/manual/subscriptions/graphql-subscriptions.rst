GraphQL and Subscriptions
=========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

GraphQL makes it easy for app developers to query for precisely the data they want from their API.
For example, let’s say we’re building a food delivery app. Here’s what the schema might look like on Postgres:

.. thumbnail:: ../../../img/graphql/manual/subscriptions/postgres-schema-for-food-delivery-app.png
  :width: 60%
  :alt: postgres schema for food delivery app

For an app screen showing a “order status” for the current user for their order,
a GraphQL query would fetch the latest order status and the location of the agent delivering the order.

.. thumbnail:: ../../../img/graphql/manual/subscriptions/order-graphql-query.png
  :width: 70%
  :alt: order graphql query

Underneath it, this query is sent as a string to a webserver that parses it,
applies authorization rules and makes appropriate calls to things like databases to fetch the data for the app.
It sends this data, in the exact shape that was requested, as a JSON.

Enter live-queries: Live queries is the idea of being able to subscribe to the latest result of a particular query.
As the underlying data changes, the server should push the latest result to the client.

On the surface this is a perfect fit with GraphQL because GraphQL clients support subscriptions that take care of
dealing with messy websocket connections. Converting a query to a live query might look as simple as replacing the word
query with subscription for the client. That is, if the GraphQL server can implement it.

.. thumbnail:: ../../../img/graphql/manual/subscriptions/order-subscription-query.png
  :width: 70%
  :alt: order subscription query
