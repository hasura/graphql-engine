Using existing database
=======================

Let's say you have an existing Heroku Postgres database with data in it, and you'd like add GraphQL on it.

.. note::

   In case you're exposing an existing database (esp. if it is production), please configure an access key
   for the console and the graphql endpoint.


Step 1: Remove the existing postgres addon in the app
-----------------------------------------------------

Head to your Heroku dashboard and delete the postgres addon:

.. image:: ../../../../img/graphql/manual/deployment/remove-heroku-postgres-addon.png

Step 2: Configure the DATABASE_URL environment variable
-------------------------------------------------------

Now configure the `DATABASE_URL` with your existing heroku postgres database URL.

.. image:: ../../../../img/graphql/manual/deployment/heroku-database-url-access.png

Step 3: Track tables and relationships
--------------------------------------

Wait for the GraphQL engine to restart and you'll see your existing tables as "untracked tables" in the console.

.. image:: ../../../../img/graphql/manual/getting-started/TrackTable.jpg
