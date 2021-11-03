.. meta::
   :description: Set default field values for Postgres using Postgres defaults in Hasura
   :keywords: hasura, docs, postgres, schema, default value, Postgres default

.. _postgres_defaults:

Postgres: Setting default values for fields using Postgres defaults
===================================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

You can set values of certain fields automatically when not explicitly passed to a fixed value, e.g. true for a boolean
field, or output of a simple SQL function, e.g. now() for a timestamp field, by setting column default values in the
table definition.

.. note::

  The Postgres default value is ignored when a value is explicitly set to the field.

**Example:** Say we have a field ``created_at`` in a table ``article`` which we want to be set to the current
timestamp whenever a new row is added to the table:

Step 1: Modify the table
------------------------

Edit the ``created_at`` field and set its default value as the SQL function ``now()``.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Open the console and head to ``Data -> [article] -> Modify``.

    Click the ``Edit`` button next to the ``created_at`` field and add ``now()`` as a default value.

    .. thumbnail:: /img/graphql/core/schema/add-default-value.png
      :alt: Modify the table in the console

  .. tab:: CLI

    :ref:`Create a migration manually <manual_migrations>` and add the following SQL statement to the ``up.sql`` file:

    .. code-block:: SQL

      ALTER TABLE ONLY "public"."article" ALTER COLUMN "created_at" SET DEFAULT now();

    Add the following statement to the ``down.sql`` file in case you need to :ref:`roll back <roll_back_migrations>` the above statement:

    .. code-block:: sql

      ALTER TABLE article ALTER COLUMN created_at DROP DEFAULT;

    Apply the migration by running:

    .. code-block:: bash

      hasura migrate apply

  .. tab:: API

    You can add a default value by using the :ref:`run_sql metadata API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "ALTER TABLE article ALTER COLUMN created_at SET DEFAULT now();"
        }
      }
    
.. admonition:: To set an auto-incrementing default value

      To set a default value as an auto-incrementing integer you first need to set up a ``sequence`` which will be the
      source of our default value.

      Let's say we have a field called ``roll_number`` which we would like to be set by default as an auto-incremented
      integer.

      Run the following SQL command to create a new sequence.

      .. code-block:: SQL

        CREATE SEQUENCE roll_number_seq;

      Now set the default value of the ``roll_number`` field as ``nextval('roll_number_seq')``.

Step 2: Run an insert mutation
------------------------------

Now if you do not pass the ``created_at`` field value while running an insert mutation on the ``article`` table, its
value will be set automatically by Postgres.

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
              "created_at": "2020-04-23T11:42:30.499315+00:00"
            }
          ]
        }
      }
    }

Also see
--------

- :ref:`sql_functions_as_default`
- :ref:`column_presets`
