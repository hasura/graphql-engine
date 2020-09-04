.. meta::
   :description: Add created at / updated at timestamps
   :keywords: hasura, docs, schema, default value, timestamps

.. _created_updated_timestamps:

Adding created_at / updated_at timestamps
=========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

We often need ``created_at`` and ``updated_at`` timestamp fields in our tables in order to indicate when an object was created or last updated.
This page explains how to add these. 

Add a created_at timestamp
--------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    On the Hasura console, click on the ``Modify`` tab of a table. When clicking on the ``+Frequently used columns`` button, 
    choose ``created_at``:

    .. thumbnail:: /img/graphql/core/schema/created-at.png
        :alt: Add a created_at time on the Hasura console

    Click the ``Add column`` button.

  .. tab:: CLI

    :ref:`Create a migration manually <manual_migrations>` and add the following SQL statement to the ``up.sql`` file:

    .. code-block:: plpgsql

      ALTER TABLE ONLY "public"."article" ADD COLUMN "created_at" TIMESTAMP DEFAULT NOW();

    Add the following statement to the ``down.sql`` file in case you need to :ref:`roll back <roll_back_migrations>` the above statement:

    .. code-block:: plpgsql

      ALTER TABLE article DROP COLUMN created_at;

    Apply the migration and reload the metadata:

    .. code-block:: bash

        hasura migrate apply
        hasura metadata reload

  .. tab:: API

    You can add a ``created_at`` timestamp by using the :ref:`run_sql metadata API <run_sql>`:

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

Add an updated_at timestamp
---------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    On the Hasura console, click on the ``Modify`` tab of a table. When clicking on the ``+Frequently used columns`` button, 
    choose ``updated_at``:

    .. thumbnail:: /img/graphql/core/schema/updated-at.png
        :alt: Add an updated_at time on the Hasura console

    Click the ``Add column`` button.

  .. tab:: CLI

    :ref:`Create a migration manually <manual_migrations>` and add the below SQL statement to the ``up.sql`` file:
    
    1. Add an ``updated_at`` timestamp field to the ``article`` table.
    2. Define a `Postgres function <https://www.postgresql.org/docs/current/sql-createfunction.html>`__ to set the ``updated_at`` field to ``NOW()``.
    3. Create a `Postgres trigger <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__ to call the defined function whenever an article is updated.

    .. code-block:: plpgsql

      ALTER TABLE ONLY "public"."article"
      ADD COLUMN "updated_at" TIMESTAMP DEFAULT NOW();

      CREATE FUNCTION trigger_set_timestamp()
      RETURNS TRIGGER AS $$
      BEGIN
        NEW.updated_at = NOW();
      RETURN NEW;
      END;
      $$ LANGUAGE plpgsql;

      CREATE TRIGGER set_timestamp
      BEFORE
      UPDATE ON article
      FOR EACH ROW
      EXECUTE PROCEDURE trigger_set_timestamp();

    Add the following statement to the ``down.sql`` file in case you need to :ref:`roll back <roll_back_migrations>` the above statement:

    .. code-block:: plpgsql

      DROP trigger set_timestamp on article;
      DROP function trigger_set_timestamp();
      ALTER TABLE article DROP COLUMN updated_at;

    Apply the migration and reload the metadata:

    .. code-block:: bash

        hasura migrate apply
        hasura metadata reload

  .. tab:: API

    You can add an ``updated_at`` timestamp by using the :ref:`run_sql metadata API <run_sql>`.

    The below SQL statement will achieve the following:

    1. Add an ``updated_at`` timestamp field to the ``article`` table.
    2. Define a `Postgres function <https://www.postgresql.org/docs/current/sql-createfunction.html>`__ to set the ``updated_at`` field to ``NOW()``.
    3. Create a `Postgres trigger <https://www.postgresql.org/docs/current/sql-createtrigger.html>`__ to call the defined function whenever an article is updated.

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
            "sql": 
              "ALTER TABLE ONLY \"public\".\"article\"
              ADD COLUMN \"updated_at\" TIMESTAMP DEFAULT NOW();

              CREATE FUNCTION trigger_set_timestamp()
              RETURNS TRIGGER AS $$
              BEGIN
                NEW.updated_at = NOW();
              RETURN NEW;
              END;
              $$ LANGUAGE plpgsql;

              CREATE TRIGGER set_timestamp
              BEFORE
              UPDATE ON article
              FOR EACH ROW
              EXECUTE PROCEDURE trigger_set_timestamp();"
        }
      }
