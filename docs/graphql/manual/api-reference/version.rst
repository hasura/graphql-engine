.. meta::
   :description: Hasura version API reference
   :keywords: hasura, docs, version API, API reference

.. _version_api_reference:

Version API Reference
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


The ``/v1/version`` is a public endpoint that responds with the current server version in JSON format.

Endpoint
--------

All requests are ``GET`` requests to the ``/v1/version`` endpoint.

API Spec
--------

Request
^^^^^^^

.. code-block:: http

   GET /v1/version HTTP/1.1


Sample response
^^^^^^^^^^^^^^^

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "version": "v1.0.0-alpha01"
   }

Disabling Version API
---------------------

The ``version`` API endpoint is public and cannot be disabled.
