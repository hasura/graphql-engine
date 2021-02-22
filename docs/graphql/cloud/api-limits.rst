.. meta::
   :description: Hasura Cloud API limits
   :keywords: hasura, docs, cloud, security, limits

.. _api_limits:

API limits
==========

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Limiting the depth and/or rate of API requests can help prevent API performance issues caused by malicious or poorly implemented queries. 

Configuring an API limit
------------------------

**Rate limits**
  Restricts number of GraphQL operations per minute. This uses a sliding window approach. This means whenever Hasura Pro receives a request, it will count the rate of that client starting from the current time to last one minute.

**Depth limits**
  Restricts a GraphQL operation based on its depth, preventing deeply nested queries.

API limits are defined by **role** (anonymous, user) and can restrict request rate, depth, or both. Unique request parameters can include IP address or session variables (*x-hasura-user-id*, *x-hasura-org-id*, etc.)

Manage API limits
-----------------

API limits can have a *global* or *per role* configuration. If an incoming request does not contain a valid role then the global limit is applied.

.. thumbnail:: /img/graphql/cloud/security/pro-tab-apilimits.png
   :alt: Hasura Cloud Console api limit tab

.. admonition:: Admin & IntrospectionQuery exemptions

  All API limits are **not** applied for the admin role, and depth limits are **NOT** applied to introspection queries

Quick-create limits
-------------------

Hasura Cloud lets you add limits with one click from the list of past operations.

.. thumbnail:: /img/graphql/cloud/security/pro-tab-apilimit-config.png
   :alt: Hasura Cloud Console create new api limit