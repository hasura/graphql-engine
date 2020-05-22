.. meta::
   :description: Set default field values using Postgres defaults
   :keywords: hasura, docs, schema, default value, Postgres default

.. _postgres_defaults:

Setting default values for fields using Postgres defaults
=========================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can set values of certain fields automatically when not explicitly passed to a fixed value, e.g. true for a boolean
field, or output of a simple SQL function, e.g. now() for a timestamp field, by setting column default values in the
table definition.

.. note::

  The Postgres default value is ignored when a value is explicitly set to the field.

**Example:** Say we have a field ``created_at`` in a table ``article`` which we want to be set to the current
timestamp whenever a new row is added to the table:

Step 1: Modify the table
------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Edit the ``created_at`` field and set its default value as the SQL function ``now()``.

    Open the console and head to ``Data -> article -> Modify``:

    .. thumbnail:: /img/graphql/manual/schema/add-default-value.png
      :alt: Modify the table in the console

  .. tab:: API

    A table can be modified via the :ref:`run_sql <run_sql>` metadata API.

.. admonition:: To set an auto-incrementing default value

  To set a default value as an auto-incrementing integer you first need to set up a ``sequence`` which will be the
  source of our default value.

  Let's say we have a field called ``roll_number`` which we would like to be set by default as an auto-incremented
  integer.

  Head to ``Data -> SQL`` on the console or call the :ref:`run_sql <run_sql>` metadata API and run the following SQL command to create a new sequence.

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
