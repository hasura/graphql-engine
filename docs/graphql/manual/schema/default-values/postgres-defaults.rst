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

Edit the ``created_at`` field and set its default value as the SQL function ``now()``.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Open the console and head to ``Data -> article -> Modify``:

    .. thumbnail:: ../../../../img/graphql/manual/schema/add-default-value.png
      :alt: Modify the table in the console

  .. tab:: API
    
    The default value of ``created_at`` field can be set to current timestamp via the Metadata API:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
           "sql": "ALTER TABLE article ALTER created_at SET DEFAULT now();"
        }
      }

.. admonition:: To set an auto-incrementing default value

  To set a default value as an auto-incrementing integer you first need to set up a ``sequence`` which will be the
  source of our default value.

  Let's say we have a field called ``roll_number`` which we would like to be set by default as an auto-incremented
  integer.

  Head to ``Data -> SQL`` and run the following SQL command to create a new sequence.

  .. code-block:: SQL

    CREATE SEQUENCE roll_number_seq;

  Now set the default value of the ``roll_number`` field as ``nextval('roll_number_seq')``.


Step 2: Run an insert mutation
------------------------------

Now if you do not pass the ``created_at`` field value while running an insert mutation on the ``article`` table, its
value will be set automatically by Postgres.

.. thumbnail:: ../../../../img/graphql/manual/schema/default-value-response.png
   :alt: Run an insert mutation

Also see
--------

- :doc:`sql-functions`
- :doc:`column-presets`
