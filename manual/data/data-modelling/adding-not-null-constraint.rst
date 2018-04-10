============================
Adding a not-null constraint
============================

In database theory, NULL is unknown or missing information. The NULL value is different from an empty string or number zero. For example, you can ask a person for an email address, if you don’t know, you use the NULL value for inserting into the email column. This indicates that the information at the time of inserting is unknown. In case the person does not have any email address, you can update it to an empty string.

The NULL value is very special. For example, NULL is not equal to anything even NULL. To check if a value is NULL or not, you use the Boolean operator IS NULL or IS NOT NULL. The expression NULL = NULL returns NULL.

``PostgreSQL`` provides the `not-null constraint <http://www.postgresqltutorial.com/postgresql-not-null-constraint/>`_ to enforce a column must not accept NULL values. It means that whenever you insert or update data, you must specify a value that is different from the NULL value.

To add a not-null constraint:

#. Open the API-Console and go to the Data section.
#. Choose the ``SQL`` section in the left panel.
#. Enter the SQL code to add a not-null constraint. For example, to add a not-null constraint over the ``title`` column of the ``article`` table, the SQL statement will look like:

   .. code-block:: sql

      ALTER  TABLE article
      ALTER COLUMN title SET NOT NULL;

#. Check the ``This is a migration`` checkbox so that a :doc:`migration <../data-migration>` is created in the ``/migrations`` directory.
#. Hit ``Run``.
