.. meta::
   :description: Set default field values using SQL functions
   :keywords: hasura, docs, schema, default value, sql function, stored procedure

.. _sql_functions_as_default:

Setting values of fields using SQL functions/stored procedures
==============================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Let's say you want to set the value of some fields as the output of some custom SQL functions or stored procedures.
This is useful to set values of fields which depend on other fields passed in the input. E.g. set
``submission_time`` of an online quiz as 1 hour from the ``start_time``.

This can be achieved by:

#. Modifying the table to allow the columns we want to be set by the SQL functions to be nullable (to allow the initial
   insert before the SQL function is run).
#. Creating an insert/update trigger on the table that calls your SQL function and sets the output values in the output
   columns.
#. Making your mutation requests without setting the SQL function output columns.

.. note::

  This approach enforces the value set in the field to always be the result of the defined SQL function even if a
  value is explicitly passed in the insert object.

**For example**, say we have a table ``sql_function_table`` with columns ``input`` and ``output`` and we would like
to set the value of the ``output`` column as the uppercased value of the string received in the ``input`` field.

Step 1: Modify the table
------------------------

Modify the table ``sql_function_table`` and make its ``output`` column nullable.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    Open the console and head to ``Data -> sql_function_table -> Modify``:

    .. thumbnail:: /img/graphql/manual/schema/modify-sql-fn-table.png
      :alt: Modify the table

  .. tab:: Via CLI

    :ref:`Create a migration manually <manual_migrations>` and add the following statement to it:

    .. code-block:: SQL

      ALTER TABLE "public"."sql_function_table" ALTER COLUMN "output" DROP NOT NULL;

    Apply the migration by running:

    .. code-block:: bash

      hasura migrate apply

  .. tab:: Via API

    You can modify a table column by using the :ref:`run_sql metadata API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "ALTER TABLE sql_function_table ALTER COLUMN output DROP NOT NULL;"
        }
      }

Step 2: Create a trigger
------------------------

The below SQL defines a ``trigger`` which will simply uppercase the value passed in the ``input`` field and set it to
the ``output`` field whenever an insert or update is made to the ``sql_function_table``:

.. code-block:: plpgsql

   CREATE FUNCTION test_func() RETURNS trigger AS $emp_stamp$
         BEGIN
             NEW.output := UPPER(NEW.input);
             RETURN NEW;
         END;
     $emp_stamp$ LANGUAGE plpgsql;

     CREATE TRIGGER test_trigger BEFORE INSERT OR UPDATE ON sql_function_table
         FOR EACH ROW EXECUTE PROCEDURE test_func();

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    Head to ``Data -> SQL`` and run the above SQL:

    .. thumbnail:: /img/graphql/manual/schema/create-trigger.png
      :alt: Create a trigger with SQL

  .. tab:: Via CLI

    :ref:`Create a migration manually <manual_migrations>` and add the above SQL to it.

    Apply the migration by running:

    .. code-block:: bash

      hasura migrate apply

  .. tab:: Via API

    You can create a trigger by using the :ref:`run_sql metadata API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "<above SQL>"
        }
      }

Step 3: Run an insert mutation
------------------------------

Run a mutation to insert an object with (input = "yabba dabba doo!", output=null) and you'll see the output
value (output="YABBA DABBA DOO!") will be set automatically.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        mutation {
          insert_sql_function_table (
            objects: [
              {input: "yabba dabba doo!"}
            ]
          ) {
            returning {
              input
              output
            }
          }
        }
      :response:
        {
          "data": {
            "insert_sql_function_table": {
              "returning": [
                {
                  "input": "yabba dabba doo!",
                  "output": "YABBA DABBA DOO!"
                }
              ]
            }
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "mutation { insert_sql_function_table (objects: [{input: \"yabba dabba doo!\"}]) { returning { input output }}}"
      }

Also see
--------

- :ref:`postgres_defaults`
- :ref:`column_presets`
