.. _psql_heroku:

How to open psql on Heroku
--------------------------

Start a psql session on the Heroku CLI.

::

  $ heroku pg:psql

Connect to your database and access the PostgreSQL terminal. 

::

  $ heroku pg:psql DATABASE

If you want to check on your databases details:

::

   $ heroku pg:info

You´ll get something like this:

.. code-block:: sql

	--HEROKU_POSTGRESQL_RED
	Plan         Standard 0
	Status       available
	Data Size    50 GB
	Tables       30
	PG Version   9.5.3
	Created      2019-08-15 11:00 PDT
	--HEROKU_POSTGRESQL_GRAY
	Plan         Standard 2
	Status       available
	Data Size    50 GB

That´s basically it! Now you´re good to go and run your SQL queries.

