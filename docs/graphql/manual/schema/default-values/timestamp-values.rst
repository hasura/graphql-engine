.. meta::
   :description: Set default timestamp field values
   :keywords: hasura, docs, schema, default value, timestamp values

.. _timestamp_values:

Setting default timestamp values
================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

We often need timestamp values in our tables, such as ``created_at`` and ``updated_at``.
This page explains how to add these values to your tables. 

Add a default timestamp value
-----------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    On the Hasura console, click on the ``Modify`` tab of a table. When clicking on the ``+Frequently used columns`` button, 
    the fields ``created_at`` and ``updated_at`` with type ``Timestamp`` and the SQL function ``now()`` will be suggested. 

    .. thumbnail:: /img/graphql/manual/schema/timestamp-values.png
        :alt: Add a timestamp value on the Hasura console

  .. tab:: CLI

    :ref:`Create a migration manually <manual_migrations>` and add the SQL statement to update a table:

    .. code-block:: SQL

      ALTER TABLE ONLY "public"."article" ADD COLUMN "created_at" TIMESTAMP DEFAULT NOW();

    Apply the migration by running:

    .. code-block:: bash

        hasura migrate apply

  .. tab:: API

    You can add a timestamp default value by using the :ref:`run_sql metadata API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
            "sql": "ALTER TABLE ONLY \"article\" ADD COLUMN \"created_at\" TIMESTAMP DEFAULT NOW();"
        }
      }
