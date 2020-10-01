Using psql using Heroku
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

.. note::
   Make sure you have `Heroku CLI <https://devcenter.heroku.com/articles/heroku-cli>`__ and `Heroku Postgres <https://devcenter.heroku.com/articles/heroku-postgresql>`__ installed.

Get database details
^^^^^^^^^^^^^^^^^^^^

::

   $ heroku pg:info -a <app-name>

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

Start psql
^^^^^^^^^^

::

  $ heroku pg:psql -a <app-name>

You can now do database operations directly on your Postgres instance.
