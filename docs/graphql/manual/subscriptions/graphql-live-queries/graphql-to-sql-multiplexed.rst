Batch multiple live-queries into one SQL query
==============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

With only ideas #1, #2 implemented we would still result in the situation where a 100k connected clients could result
in a proportional load of 100k postgres queries to fetch the latest data (let’s say if 100k updates happen, 1 update
relevant to each client).

However, considering that we have all the application-user level session variables available at the API layer, we can
actually create a single SQL query to re-fetch data for a number of clients all at once!

Let’s say that we have clients running a subscription to get the latest order status and the delivery agent location.
We can create a “relation” in-query that contains all the query-variables (the order IDs) and the session variables
(the user IDs) as different rows. We can then “join” the query to fetch the actual data with this relation to ensure
that we get the latest data for multiple clients in a single response. Each row in this response contains the final
result for each user. This will allow fetching the latest result for multiple users at the same time, even though the
parameters and the session variables that they provide are completely dynamic and available only at query-time.

.. thumbnail:: ../../../../img/graphql/manual/subscriptions/graphql-2-sql-multiplexed.png
  :alt: graphql to sql multiplexed
