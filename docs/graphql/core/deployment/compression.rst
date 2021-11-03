.. meta::
   :description: Use HTTP compression with Hasura GraphQL engine
   :keywords: hasura, docs, deployment, http compression

.. _http_compression:

HTTP Compression
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Hasura GraphQL Engine supports HTTP compression.
The server looks for the ``Accept-Encoding`` header in request.
If the header contains ``gzip`` then the server uses `Gzip <https://en.wikipedia.org/wiki/Gzip>`__ compression.
Also, the server sets the ``Content-Encoding`` response header value to ``gzip``.

**Only responses from "/v1/query" and "/v1/graphql" endpoints are compressed.**
