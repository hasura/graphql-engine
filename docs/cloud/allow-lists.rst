.. meta::
   :description: Hasura Pro allow lists
   :keywords: hasura, docs, cloud, security, allow

.. _allow_lists:

Allow lists
===========

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can specify a list of safe operations (GraphQL queries, mutations or subscriptions) for your project. This list restricts your project's GraphQL Engine to execuse only queries that are present in the list.

Manage the allow list
---------------------

The manager view offers inspection, export, or removal of operations in the allow list:

.. thumbnail:: /img/cloud/security/pro-tabs-allowlist.png
   :alt: Hasura Cloud Console allow list tab

Quick-create allowed operations
-------------------------------

This Pro feature lets you add to the allow list with one click from the record of past operations. (With Core, allow lists must be `managed manually <https://hasura.io/docs/1.0/graphql/manual/deployment/allow-list.html#enable-allow-list>`_.)

.. thumbnail:: /img/cloud/security/allowlist-add-new-op.png
   :alt: Hasura Cloud Console create new allowed operation