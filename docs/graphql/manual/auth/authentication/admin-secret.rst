.. meta::
   :description: Use authenticaton with webhooks in Hasura
   :keywords: hasura, docs, authentication, auth, admin secret

.. _admin_secret:

Authentication using admin secret
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

By default, all tables created with Hasura come with an ``admin`` role that has full read and write access:

.. thumbnail:: ../../../../img/graphql/manual/auth/console-admin-permissions.png
   :alt: Admin permissions

An admin secret should be added to prevent admin access for the Hasura console through third parties. 
The admin secret should be a secure random string, and it should be stored like a proper secure password.

.. note::
    If you're looking at adding access control rules for roles other than ``admin``, head to :ref:`access control <authorization>`.

Configuring admin secret
------------------------

The way to configure an admin secret depends on your deployment method.

See :ref:`these guides <securing_graphql_endpoint>` for configuring an admin secret with different deployment methods.
