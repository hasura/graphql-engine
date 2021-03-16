.. meta::
   :description: Manage RESTified endpoints with the Hasura metadata API
   :keywords: hasura, docs, metadata API, API reference, RESTified endpoints

.. _metadata_api_restified_endpoints:

Metadata API Reference: RESTified GraphQL Endpoints (v2.0 and above)
====================================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Add/Remove a RESTified GraphQL endpoint to Hasura GraphQL engine.

.. admonition:: Supported from

  The metadata API is supported for versions ``v2.0.0`` and above and replaces the older
  :ref:`schema/metadata API <schema_metadata_apis>`.

.. _metadata_create_rest_endpoint:

create_rest_endpoint
--------------------

``create_rest_endpoint`` is used to associate a URL template with a query.

An example request as follows:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "create_rest_endpoint",
       "args": {
           "name": "example-name",
           "url": "example",
           "methods": ["POST","PUT","PATCH"],
           "definition": {
               "query": {
                 "query_name": "example_mutation",
                 "collection_name": "test_collection"
               }
           },
           "comment": "some optional comment"
       }
   }


.. _metadata_create_rest_endpoint_syntax:

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - Text
     - A unique identifier for the endpoint
   * - url
     - true
     - :ref:`EndpointUrl`
     - URL of the REST endpoint
   * - methods
     - true
     - :ref:`EndpointMethods`
     - Non-Empty case sensitive list of supported HTTP Methods
   * - definition
     - true
     - :ref:`EndpointDef`
     - Definition for the REST endpoint
   * - comment
     - false
     - Text
     - comment

.. admonition:: Supported from

  RESTified endpoints are supported from versions ``v2.0.0-alpha.1`` and above.

.. _metadata_drop_rest_endpoint:

drop_rest_endpoint
------------------

``drop_rest_endpoint`` is used to delete an existing RESTified GraphQL Endpoint.

An example request as follows:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "drop_rest_endpoint",
       "args": {
           "name": "name_of_the_endpoint"
       }
   }

.. _metadata_drop_rest_endpoint_syntax:

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - Text
     - URL of the RESTified endpoint

.. admonition:: Supported from

  RESTified endpoints are supported from versions ``v2.0.0-alpha.1`` and above.
