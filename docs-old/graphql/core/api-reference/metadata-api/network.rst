.. meta::
   :description: Manage network options with the Hasura metadata API
   :keywords: hasura, docs, metadata API, API reference, network options, TLS, allowlist

.. _metadata_api_network_options_:

========================================================================
 Metadata API Reference: Network Options (v2.0 and above)
========================================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
============

Here's the API to modify any ``Network`` metadata. One of the options is to manage a ``TLS allowlist``.

TLS Allowlist
=============

The TLS Allowlist represents a set of services that are permitted to use self-signed certificates - primarily intended for use in development
and staging environments, services can be whitelisted by a ``host``, and optionally (service id) ``port``.

.. _metadata_add_host_to_tls_allowlist:

add_host_to_tls_allowlist
=============================

``add_host_to_tls_allowlist`` is used to add any string

This API could be supplied with just the hostname in the ``args`` field
of the request instead of the complete object.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "add_host_to_tls_allowlist",
       "args": {
           "host": "graphql.hasura.io",
           "suffix": "core.graphql",
           "permissions": ["self-signed"]
       }
   }

.. _add_host_to_tls_allowlist_syntax:

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - host
     - true
     - ``String``
     - the hostname/domain of the endpoint
   * - suffix
     - false
     - ``String``
     - suffix for the service (this is usually reserved for the service port number)
   * - permissions
     - false
     - ``[String]``
     - Can be only ``["self-signed"]`` until more permissions are supported. "self-signed" allows self-signed, name mismatches, and non-X509.V3 certificates.


.. _metadata_drop_host_from_tls_allowlist:

drop_host_from_tls_allowlist
================================

``drop_host_from_tls_allowlist`` is used to drop an endpoint from the TLS allowlist.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "drop_host_from_tls_allowlist",
       "args": {
           "host": "graphql.hasura.io"
       }
   }

.. _drop_host_from_tls_allowlist_syntax:

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - host
     - true
     - ``String``
     - The hostname/domain of the endpoint that was previously added to the allowlist
