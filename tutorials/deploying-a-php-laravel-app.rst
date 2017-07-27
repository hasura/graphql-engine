:orphan:

.. meta::
   :description: A tutorial on deploying a Laravel PHP/Apache web application using Hasura complete with migrations and a postgres database
   :keywords: hasura, docs, tutorials, php, apache, web-application, laravel, migrations, postgres
   :content-tags: php, apache, deployment, web-application

Deploying a Laravel PHP app
===========================

.. rst-class:: featured-image
.. image:: ../img/php-apache.png
   :height: 0px
   :width: 0px

This tutorial will take you over deploying a Laravel PHP application on Hasura.

Benefits of using Hasura to deploy and host your Laravel app:
1. A Hasura project comes with a pre-configured Postgres that's ready to be used
2. `git push hasura master` inside your laravel app will deploy your application to your server
3. Migrations are automatically handled whenever you update and deploy your application to the server!

Basic deployment
----------------
Follow the 4 steps below so that you can start off and deploy a Laravel app
within minutes. Refer to the next section on :ref:`local development`, to connect to
the Postgres database when you're developing and testing locally.

Step 1: Get a hasura project and set up `hasuractl`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sign up on http://dashboard.hasura.io and get yourself a Hasura project.
Creating a hasura project will give you a domain. Something like: `project42.hasura-app.io`
You will also get an email with your `admin` credentials for your project console and your
database.

.. code::

   Postgres username: admin
   Postgres password: password

Step 2: Install hasuractl and initialise a Laravel project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is the file structure that will be setup:

.. code::

   /
   - Dockerfile
   - app/ # contains your Laravel project
   ---- .env
   ---- composer.json
   ---- artisan
   ---- app/
   ---- database/
   ---- config/
   ---- routes/
   ---- ... # and more
   - .gitignore
   - .git


Step 3: Configure your laravel `.env` file and set the postgres password
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the postgres password from Step 1 above, and set that in your `.env` file.

.. code::

   DB_PASSWORD=password


Step 3: Use hasuractl to add your SSH key to the Hasura project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

xxxxx

Step 4: `git push` and you're done!
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

xxxx

.. _local-development:
Local development
-----------------

Considering that the postgres database is already on the Hasura project cluster, when you are
developing on your own mahcine, on your application you might want to connect to the database too.

Step 1: Setup a secure tunnel to your database
----------------------------------------------

xxxx

Step 2: Change your `.env` environment variables
------------------------------------------------

Change `DB_HOST` to `localhost`. The original value would have been `postgres.hasura`.

.. code::

   DB_HOST=localhost

Step 3: Run `php artisan serve`!
--------------------------------

And everything works. :)
