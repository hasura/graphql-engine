.. meta::
   :description: Configure authenticaton with webhooks in Hasura
   :keywords: hasura, docs, authentication, auth, webhook, configure

.. _configure_auth_webhooks:

Configuring webhook mode
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This page explains how to configure webhook mode in Hasura.

Configuring webhook mode
------------------------

* You can configure Hasura to run in webhook mode by running the GraphQL engine with the ``--auth-hook`` flag or the ``HASURA_GRAPHQL_AUTH_HOOK`` environment variable (see :ref:`GraphQL engine server options <server_flag_reference>`), the value of which is the webhook endpoint.

* You can configure Hasura to send either a ``GET`` or a ``POST`` request to your auth webhook. The default configuration is ``GET`` and you can override this with ``POST`` by using the ``--auth-hook-mode`` flag or the ``HASURA_GRAPHQL_AUTH_HOOK_MODE`` environment variable (*in addition to those specified above; see* :ref:`GraphQL engine server options <server_flag_reference>`).

.. note::

  If you are running Hasura using Docker, ensure that the Hasura Docker container can reach the webhook.
  See :ref:`this page <docker_networking>` for Docker networking.

