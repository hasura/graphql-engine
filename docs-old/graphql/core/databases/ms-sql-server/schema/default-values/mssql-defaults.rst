.. meta::
   :description: Set default field values for MS SQL Server using MS SQL Server defaults in Hasura
   :keywords: hasura, docs, ms sql server, schema, default value, MS SQL Server default

.. _mssql_defaults:

MS SQL Server: Setting default values for fields using MS SQL Server defaults
=============================================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

You can set values of certain fields automatically when not explicitly passed to a fixed value, e.g. true for a boolean
field, or output of a simple SQL function, e.g. GETDATE() for a timestamp field, by setting column default values in the
table definition.

.. note::

  The MS SQL Server default value is ignored when a value is explicitly set to the field.

**Example:** Say we have a field ``created_at`` in a table ``article`` which we want to be set to the current
timestamp whenever a new row is added to the table:

Step 1: Modify the table
------------------------

Edit the ``created_at`` field and set its default value as the SQL function ``GETDATE()``.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Open the console and head to ``Data -> [article] -> Modify``.

    Click the ``Edit`` button next to the ``created_at`` field and add ``GETDATE()`` as a default value.

    .. thumbnail:: /img/graphql/core/schema/mssql-add-default-value.png
      :alt: Modify the table in the console
      :width: 800px

  .. tab:: API

    You can add a default value by using the :ref:`mssql_run_sql` schema API:

    .. code-block:: http

      POST /v2/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "mssql_run_sql",
        "args": {
          "source": "<db_name>",
          "sql": "ALTER TABLE article ADD CONSTRAINT DF_article DEFAULT GETDATE() FOR created_at;"
        }
      }

.. admonition:: To set an auto-incrementing default value

      To set a default value as an auto-incrementing integer you first need to set up a ``sequence`` which will be the
      source of our default value.

      Let's say we have a field called ``roll_number`` which we would like to be set by default as an auto-incremented
      integer.

      Run the following SQL command to create a new sequence.

      .. code-block:: SQL

        CREATE SEQUENCE roll_number_seq AS INT START WITH 0 INCREMENT BY 1;

      Now set the default value of the ``roll_number`` field as ``DEFAULT (NEXT VALUE FOR roll_number_seq)``.

Step 2: Run an insert mutation
------------------------------

Now if you do not pass the ``created_at`` field value while running an insert mutation on the ``article`` table, its
value will be set automatically by MS SQL Server.

.. graphiql::
  :view_only:
  :query:
    mutation {
      insert_article(
        objects: [
          {
            title: "GraphQL manual",
            author_id: 11
          }
        ]) {
        returning {
          id
          title
          created_at
        }
      }
    }
  :response:
    {
      "data": {
        "insert_article": {
          "returning": [
            {
              "id": 12,
              "title": "GraphQL manual",
              "created_at": "2022-01-12T13:07:08.897"
            }
          ]
        }
      }
    }

Also see
--------

- :ref:`mssql_column_presets`
