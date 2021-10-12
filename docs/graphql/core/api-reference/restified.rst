.. meta::
   :description: Hasura RESTified GraphQL API reference
   :keywords: hasura, docs, REST API, API reference

.. _restified_api_reference:

RESTified GraphQL Endpoints API Reference
=========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The RESTified GraphQL API allows for the use of a REST interface to saved GraphQL queries and mutations.

Users specify the query or mutation they wish to make available, as well a URL template.
Segments of the URL template can potentially capture data to be used as GraphQL variables.

See :ref:`api_restified_endpoints` for information on how to work with endpoints through the metadata apis.

.. admonition:: Supported from

  RESTified endpoints are supported from versions ``v2.0.0-alpha.1`` and above.

Endpoint
--------

Requests are made to ``/api/rest/**`` endpoints.

By default these are:

* ``GET``, ``POST`` requests for queries, and
* ``POST`` requests for mutations

Although not in the REST spirit, ``POST`` requests are allowed by default for
non-mutation queries since some HTTP clients may not be able to supply a JSON
body for GET requests.

If required, users may explicitly configure the HTTP methods allowed per endpoint.


API Spec
--------

Request
^^^^^^^

Endpoints are determined by the URL templates:

* Simple URLs such as ``/s1/s2/s3`` match literally.
* Segments starting with a ``:`` treat these parts of the path like wildcards and use the name to assign a variable. For example: ``/s1/:id/s3``.


The request expects a normal REST style request to the endpoint.

Variables can be supplied via route patterns, url query variables, and request body JSON object keys.

* JSON Body Object values are passed directly to the associated query with no additional validation or type-coersion.
* Route variables and Query parameters are interpreted as scalars according to the variables types in the associated query and passed as JSON data through the query variables:

  * Missing nullable variables are interpreted as ``NULL``
  * Boolean variables are interpreted as ``Boolean``
  * Boolean flags without values e.g. ``/api/rest/myquery?mybool`` are interpreted as ``true``
  * String variables are interpreted as ``String``
  * UUID variables are interpreted as ``String``
  * ID variables are interpreted as ``String``
  * Number, Int, Float, and Double variables are interpreted as ``Number``
  * **All other types or mismatches currently report variable type errors**


When making a request to this API only one endpoint should match. If multiple endpoints match your request this is considered an error and will report so via a 500 status code. If endpoints exist with your path, but none matching your HTTP method then a 405 error will be returned
listing the methods that you can use.

Sample requests
***************


.. code-block:: http

   GET /api/rest/simple_query/1 HTTP/1.1

.. code-block:: http

   POST /api/rest/complicated_mutation/2?time=now HTTP/1.1
   Content-Type: application/json

   {
        "user": {"name": "Simon"}
   }


Response
^^^^^^^^

The response is determined by the saved query. The response will be the same as if you had made the query directly in the GraphQL console.

See the :ref:`api_reference_graphql` for more details.
