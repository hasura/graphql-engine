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

- The query does not make use of remote schemas or remote joins
- The query and any related user permissions do not make use of session variables
- The response JSON is under 100KB in size

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
