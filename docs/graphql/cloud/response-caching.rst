.. meta::
   :description: Query response caching in Hasura Cloud
   :keywords: hasura, docs, cloud, response, caching

.. _response_caching:

Query response caching
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Hasura Cloud provides support for caching query responses, in order to
improve performance for queries which are executed frequently.

Cached responses are stored in for a period of time in a LRU (least-recently
used) cache, and removed from the cache as needed based on usage.

A query's response can be cached only if the following conditions hold:

- The query does **not** make use of ``remote schemas`` or ``remote joins``.
- The query **isn't** an ``Action`` that has ``forward_client_headers`` (see :ref:`ActionDefinition`) set to ``true``.
- The response JSON is **under** 100KB in size

.. admonition:: Support

  Query response caching is available for Hasura Cloud projects on the ``Standard`` (pay-as-you-go) tier and above.

Enable caching
--------------

In order to enable caching for a query response, or to return an existing
response from the cache (if one exists), simply add the ``@cached`` directive
to your query:

.. code-block:: graphql

   query MyCachedQuery @cached {
     users {
       id
       name
     }
   }

If the response was cached successfully, the HTTP response will include a
``X-Hasura-TTL`` header, whose value indicates the maximum number of seconds
for the returned response to remain in the cache.

Controlling cache lifetime
--------------------------

The maximum lifetime of an entry in the cache can be controlled using the ``ttl``
argument to the ``@cached`` query directive. The value is an integer number of seconds:

.. code-block:: graphql

   query MyCachedQuery @cached(ttl: 120) {
     users {
       id
       name
     }
   }

By default, a response will be cached with a maximum lifetime of 60 seconds.
The maximum allowed value is 300 seconds (5 minutes).

Rate Limiting
-------------

Cache writes are rate limited, with a rate depending on your plan. The rate
limit is based on the total number of bytes written to the cache in a sliding
window. If you exceed the rate limit, the HTTP response will indicate this
with a 429 (Too Many Requests) error response.

Session variables
-----------------

Queries using session variables are able to be cached.

Please note:

* A session variable will only influence the cache key for a query if it referenced by the execution plan.
  In practice this means that session variables are only factored into cache keys if they are referenced
  in the permissions for a query.
  See https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html

Response headers
----------------

When you enable caching for a query, the following headers should be returned in the HTTP response:

* ``X-Hasura-Query-Cache-Key`` - Key for cached query response, unique to this query
* ``X-Hasura-Query-Family-Cache-Key`` - Key for the family of queries (ignores variable values)
* ``Cache-Control`` - Value: ``max-age={SECONDS}`` - Seconds until cache expires for query

These can be used by your application as you see fit, as well as by the cache clearing endpoints.

Clearing items from the cache
-----------------------------

A set of endpoints exist to clear items from the cache for the current project:

* ``POST /pro/cache/clear`` -- Clears all
* ``POST /pro/cache/clear?key={HASH}`` -- Clears key hash
* ``POST /pro/cache/clear?family={FAMILY}`` -- Clears items that match query family (ignoring variables)
