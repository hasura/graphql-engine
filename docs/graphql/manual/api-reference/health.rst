.. meta::
   :description: Hasura health API reference
   :keywords: hasura, docs, health API, API reference

.. _health_api_reference:

Health check API Reference
==========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Health API is a public endpoint which gives info on the server health.

Endpoint
--------

All requests are ``GET`` requests to the ``/healthz`` endpoint.

API Spec
--------

Request
^^^^^^^

.. code-block:: http

   GET /healthz HTTP/1.1

Response
^^^^^^^^

Depending on the server health status any of the following responses can be returned:

.. list-table::
  :header-rows: 1

  * - Server condition
    - HTTP Status
    - Message
  * - All healthy
    - 200
    - OK
  * - Serving requests but some metadata objects are inconsistent/not-available
    - 200
    - WARN: inconsistent objects in schema
  * - Unhealthy
    - 500
    - ERROR

.. note::

  If there are metadata inconsistencies, you should use the Hasura console or the
  `get_inconsistent_metadata <schema-metadata-api/manage-metadata.html#get-inconsistent-metadata>`_ API to find out what
  the inconsistent objects are and resolve them.


Sample response
***************

.. code-block:: http

   HTTP/1.1 200 OK


Disabling Health check API
--------------------------

The ``healthz`` API endpoint is public and cannot be disabled
