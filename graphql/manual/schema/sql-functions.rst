Running custom SQL functions
============================

Let's say you want to run custom SQL functions or stored procedures via GraphQL. Hasura GraphQL engine allows you to
run your SQL functions via mutations.

At a high-level:

#. Create a table with columns that capture the input, and columns that capture the output of the SQL function along
   with an auto-generated id column
#. Create an insert/update trigger on that table that calls your SQL function and sets the output values in the output
   columns
#. Use the insert or update mutation to pass values to the SQL function by setting values in the input columns and
   getting output from the function via the ``returning`` field

Here's a simple example:

Create a table
--------------

Create the following table: ``sql_function_table (id serial, input text, output text nullable)``

Head to ``Data -> Create table``

.. image:: ../../../img/graphql/manual/schema/create-sql-fn-table.png

Create a trigger
----------------

The below SQL defines a ``trigger`` which will simply uppercase the value passed in the ``input`` field and set it to
the ``output`` field whenever an insert or update is made to the ``sql_function_table``.

.. code-block:: plpgsql

   CREATE FUNCTION test_func() RETURNS trigger AS $emp_stamp$
         BEGIN
             NEW.output := UPPER(NEW.input);
             RETURN NEW;
         END;
     $emp_stamp$ LANGUAGE plpgsql;

     CREATE TRIGGER test_trigger BEFORE INSERT OR UPDATE ON sql_function_table
         FOR EACH ROW EXECUTE PROCEDURE test_func();

Head to ``Data -> SQL`` and run the above SQL:

.. image:: ../../../img/graphql/manual/schema/create-trigger.png

Run an insert mutation
----------------------

Run a mutation to insert an object with (input = "yabba dabba doo!", output=null) and you'll get a return
value (output="YABBA DABBA DOO!")

.. graphiql::
  :view_only: true
  :query:
    mutation {
      insert_sql_function_table (
        objects: [
          {input: "yabba dabba doo!"}
        ]
      ) {
        returning {
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
              "output": "YABBA DABBA DOO!"
            }
          ]
        }
      }
    }
