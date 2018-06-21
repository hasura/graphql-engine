Creating custom SQL functions
=============================

Let's say you want to run custom SQL functions or stored procedures via GraphQL.

Hasura allows you to run your SQL functions via mutations. At a high-level:

#. Create a table with columns that capture the input, and columns that capture the output of the SQL function along with an auto-generated id column
#. Create an insert/update trigger on that table that calls your SQL function and sets the output values in the output columns
#. Use the insert or update mutation to pass values to the SQL function by setting values in the input columns and getting output from the function via the ``returning`` field

Here's a simple example:

Create a table
--------------

``sql_function_table (id serial, input text, output text nullable)``

.. note::

   TODO

Create a trigger
----------------

.. note::

   TODO

.. code-block:: sql

   CREATE FUNCTION test_fun() RETURNS trigger AS $emp_stamp$
         BEGIN
             NEW.output := NEW.input;
             RETURN NEW;
         END;
     $emp_stamp$ LANGUAGE plpgsql;

     CREATE TRIGGER test_trigger BEFORE INSERT OR UPDATE ON sql_function_table
         FOR EACH ROW EXECUTE PROCEDURE test_fun();

Run an insert mutation
----------------------

Run a mutation to insert an object with (input = "test", output=null) and you'll get a return value (output="test")
.. note::

   TODO
