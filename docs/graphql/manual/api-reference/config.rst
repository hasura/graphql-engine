.. _config_api_reference:

Config API Reference
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Config API is an admin only endpoint which gives info on the server
configuration.

Endpoint
--------

All requests are ``GET`` requests to the ``/v1alpha1/config`` endpoint.

API Spec
--------

Request
^^^^^^^

.. code-block:: http

   GET /v1alpha1/config HTTP/1.1
   X-Hasura-Role: admin



Sample response
^^^^^^^^^^^^^^^

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {
     "version": "v1.0.0-beta.3",
     "is_admin_secret_set": true,
     "is_auth_hook_set": false,
     "is_jwt_set": true,
     "jwt": {
       "claims_namespace": "https://hasura.io/jwt/claims",
       "claims_format": "json"
     }
   }

Disabling Config API
--------------------

The ``enabled-apis`` flag or the ``HASURA_GRAPHQL_ENABLED_APIS`` env var can be
used to enable/disable this API. By default, this API is enabled. To disable it,
you need to explicitly state that this API is not enabled. i.e. remove it from
the list of enabled APIs.

.. code-block:: bash

   # enable only graphql & metadata apis, disable config
   --enabled-apis="graphql,metadata"
   HASURA_GRAPHQL_ENABLED_APIS="graphql,metadata"

See :doc:`../deployment/graphql-engine-flags/reference` for info on setting the above flag/env var
