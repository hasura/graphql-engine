When do we refetch?
===================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

We experimented with several methods of capturing events
from the underlying Postgres database to decide when to refetch queries.

1) Listen/Notify: Requires instrumenting all tables with triggers, events consumed by consumer (the web-server)
   might be dropped in case of the consumer restarting or a network disruption.

2) WAL: Reliable stream, but LR slots are expensive which makes horizontal scaling hard, and are often not available
   on managed database vendors. Heavy write loads can pollute the WAL and will need throttling at the application layer.

After these experiments, we’ve currently fallen back to interval based polling to refetch queries.
So instead of refetching when there is an appropriate event, we refetch the query based on a time interval.
There were two major reasons for doing this:

1) Mapping database events to a live query for a particular client is possible to some extent when the declarative
   permissions and the conditions used in the live queries are trivial (like `order_id` = 1 and `user_id` =
   cookie.`session_id`) but becomes intractable for anything complicated (say the query uses `'status' ILIKE 'failed_%'`).
   The declarative permissions can also sometimes span across tables.

2) For any application unless the write throughput is very small,
   you’ll end up throttling/debouncing events over an interval anyway.

The tradeoff with this approach is latency when write-loads are small. Refetching can be done immediately,
instead after X ms. This can be alleviated quite easily, by tuning the refetch interval and the batch size appropriately.
So far we have focussed on removing the most expensive bottleneck first, the query refetching.
