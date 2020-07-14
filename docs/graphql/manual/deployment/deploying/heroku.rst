.. meta::
   :description: Deploy Hasura GraphQL engine with Heroku
   :keywords: hasura, docs, deployment, heroku

.. _deploy_heroku:

Run Hasura GraphQL engine on Heroku
===================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide shows how to deploy Hasura GraphQL engine on Heroku. If you're looking to take Hasura for a spin, check out the :ref:`Heroku one-click deployment <heroku_one_click>`.
If you're looking for a production environment, refer to the subsequent sections.

.. _heroku_one_click:

One-click deployment
--------------------

You can deploy Hasura on Heroku within a few seconds. Follow these steps to do so:

Step 1: Deploy to Heroku
^^^^^^^^^^^^^^^^^^^^^^^^

.. image:: https://camo.githubusercontent.com/83b0e95b38892b49184e07ad572c94c8038323fb/68747470733a2f2f7777772e6865726f6b7563646e2e636f6d2f6465706c6f792f627574746f6e2e737667
  :width: 200px
  :alt: heroku_deploy_button
  :class: no-shadow
  :target: https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku

.. note::
   If you don't have an account on Heroku, you need to sign up on Heroku. You won't need a credit card, and once you
   sign up you'll be redirected to your Heroku app creation page automatically.

.. thumbnail:: /img/graphql/manual/guides/heroku-app.png
   :alt: Deploy to Heroku 

Note that **Heroku's free Postgres add-on** is also automatically provisioned!

Step 2: Open the Hasura console
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

That's it!  Head to ``https://<YOUR_HEROKU_APP>.herokuapp.com`` and open your app.
You should see the Hasura console.

.. thumbnail:: /img/graphql/manual/guides/heroku-app-deployed.png
   :alt: Open the Hasura console

Step 3: Hello World (GraphQL or event triggers)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Make your :ref:`first graphql query <first_graphql_query>`

OR

Set up your :ref:`first event trigger <first_event_trigger>`

.. _heroku_new_app:

Deploying with a new Postgres instance
--------------------------------------

These are some sample deployment instructions while creating a new app.

Step 1: Create an app with **--stack=container**
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use the `Heroku CLI <https://devcenter.heroku.com/articles/heroku-cli>`__ to create a new Heroku app. Let's call
the app ``graphql-on-postgres``.

.. code-block:: bash

  # Replace graphql-on-postgres with whatever you'd like your app to be called
  $ heroku create graphql-on-postgres --stack=container

  Creating ⬢ graphql-on-postgres... done, stack is container
  https://graphql-on-postgres.herokuapp.com/ | https://git.heroku.com/graphql-on-postgres.git

**Note**:

- ``HEROKU_GIT_REMOTE``: `https://git.heroku.com/graphql-on-postgres.git`
- ``HEROKU_APP_URL``: `https://graphql-on-postgres.herokuapp.com/`

Step 2: Create the Heroku Postgres add-on
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create the Postgres add-on in your Heroku app.

.. code-block:: bash

  $ heroku addons:create heroku-postgresql:hobby-dev -a graphql-on-postgres

  Creating heroku-postgresql:hobby-dev on ⬢ graphql-on-postgres... free
  Database has been created and is available
   ! This database is empty. If upgrading, you can transfer
    ! data from another database with pg:copy
    Created postgresql-angular-20334 as DATABASE_URL
    Use heroku addons:docs heroku-postgresql to view documentation

Step 3: **git push** to deploy
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Remember to change ``HEROKU_GIT_REMOTE`` to your git remote below. In our case:
``https://git.heroku.com/graphql-on-postgres.git``.

.. code-block:: bash

  $ git init && git add .
  $ git commit -m "first commit"
  $ git remote add heroku HEROKU_GIT_REMOTE
  $ git push heroku master

Visit ``https://graphql-on-postgres.herokuapp.com`` (replace ``graphql-on-postgres`` with your app name) and
you should see the Hasura console.

.. _heroku_existing_db:

Deploying using an existing database
------------------------------------

Let's say you have an existing `Heroku Postgres <https://www.heroku.com/postgres>`__ database with data in it, and you'd
like add GraphQL on it.

.. note::

   In case you're exposing an existing database (esp. if it is production), please configure an admin secret key
   for the console and the GraphQL endpoint.

Step 1: Deploy Hasura on Heroku
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Deploy Hasura on Heroku by clicking on this button:

.. image:: https://camo.githubusercontent.com/83b0e95b38892b49184e07ad572c94c8038323fb/68747470733a2f2f7777772e6865726f6b7563646e2e636f6d2f6465706c6f792f627574746f6e2e737667
  :width: 200px
  :alt: heroku_deploy_button
  :class: no-shadow
  :target: https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku

Follow the Heroku instructions to deploy, check if the Hasura console loads up when you click on **View app** and then head
to the **Manage App** screen on your Heroku dashboard.

This will deploy Hasura with a free Postgres add-on automatically provisioned.

Step 2: Remove the created Postgres add-on in the app
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Head to your Heroku dashboard and delete the Postgres add-on created in the previous step:

.. thumbnail:: /img/graphql/manual/deployment/remove-heroku-postgres-addon.png
   :alt: Delete the Postgres add-on

Step 3: Configure environment variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now configure the ``DATABASE_URL`` with your existing Heroku Postgres database URL and a ``HASURA_GRAPHQL_ADMIN_SECRET``
if you want to secure your endpoint.

.. thumbnail:: /img/graphql/manual/deployment/heroku-database-url-access.png
   :alt: Configure environment variables

.. note::

  The Hasura GraphQL engine needs access permissions to your Postgres database as described in
  :ref:`Postgres permissions <postgres_permissions>`.

Step 4: Track tables and relationships
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Wait for the GraphQL engine to restart and then see :ref:`schema_existing_db` to enable GraphQL
over the database.

Configuring the database setup manually
---------------------------------------

You can set up the Hasura GraphQL engine as a "git push to deploy" app on
`Heroku <https://www.heroku.com/platform>`__ and connect it to a `Heroku Postgres <https://www.heroku.com/postgres>`__
instance. 

Step 1: Clone the Hasura GraphQL engine Heroku app
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The Hasura app with Heroku buildpack/configuration is available at:

https://github.com/hasura/graphql-engine-heroku

Step 2: Configure the database URL
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Edit the command in the ``Dockerfile`` to change which database the Hasura GraphQL engine connects to.
By default, it connects to the primary database in your app which is available at ``DATABASE_URL``.

.. code-block:: dockerfile
   :emphasize-lines: 6

   FROM hasura/graphql-engine:latest

   # Change $DATABASE_URL to your Heroku Postgres URL if you're not using
   # the primary Postgres instance in your app
   CMD graphql-engine \
     --database-url $DATABASE_URL \
     serve \
     --server-port $PORT \
     --enable-console

Read about more configuration options :ref:`here <server_flag_reference>`.

.. note::

  Hasura GraphQL engine needs access permissions to your Postgres database as described in
  :ref:`Postgres permissions <postgres_permissions>`.


.. _heroku_secure:

Securing the GraphQL endpoint
-----------------------------

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an admin secret key.


Add the HASURA_GRAPHQL_ADMIN_SECRET env var
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Head to the config-vars URL on your Heroku dashboard and set the ``HASURA_GRAPHQL_ADMIN_SECRET`` environment variable.

.. thumbnail:: /img/graphql/manual/deployment/secure-heroku.png
   :alt: Add an admin secret

Setting this environment variable will automatically restart the dyno. Now when you access your console, you'll be
prompted for the admin secret key.

.. thumbnail:: /img/graphql/manual/deployment/access-key-console.png
   :alt: Prompt for the admin secret

.. note::

  The ``HASURA_GRAPHQL_ADMIN_SECRET`` should never be passed from the client to Hasura GraphQL engine as it would
  give the client full admin rights to your Hasura instance. See :ref:`auth` for information on
  setting up authentication.


(optional) Use the admin secret with the CLI
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In case you're using the CLI to open the Hasura console, use the ``admin-secret`` flag when you open the console:

.. code-block:: bash

   hasura console --admin-secret=myadminsecretkey

.. _heroku_logs:

GraphQL engine server logs
--------------------------

You can use the `Heroku CLI <https://devcenter.heroku.com/articles/heroku-cli>`__ to check the logs
of the Hasura GraphQL engine deployed on Heroku:

.. code-block:: bash

   $ heroku logs --app <hasura-graphql-engine-app-name>

   2018-10-09T11:18:21.306000+00:00 app[web.1]: {"timestamp":"2018-10-09T11:18:21.305+0000", "level":"info", "type":"http-log", "detail":{"status":200, "query_hash":"48c74f902b53a886f9ddc1b7dd12a4a6020d70c3", "http_version":"HTTP/1.1", "query_execution_time":9.477913e-3, "request_id":"b7bb6fb3-97b3-4c6f-a54a-1e0f71a190e9", "url":"/v1/graphql", "user":{"x-hasura-role":"admin"}, "ip":"171.61.77.16", "response_size":15290, "method":"POST", "detail":null}}
   ...

**See:**

- https://devcenter.heroku.com/articles/logging for more details on logging on Heroku.

- :ref:`hge_logs` for more details on Hasura logs

.. _heroku_update:

Updating GraphQL engine
-----------------------

This guide will help you update the Hasura GraphQL engine running on Heroku. This guide assumes that you already have a
Hasura GraphQL engine running on Heroku.

The current latest version is:

.. raw:: html

   <code>hasura/graphql-engine:<span class="latest-release-tag">latest</span></code>

Follow these steps to update Hasura GraphQL engine to the lastest version:

Step 1: Clone the Hasura GraphQL engine Heroku app
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The Hasura app with Heroku buildpack/configuration is available at:
https://github.com/hasura/graphql-engine-heroku.

Clone the above repository.

.. code-block:: bash

   git clone https://github.com/hasura/graphql-engine-heroku
   cd graphql-engine-heroku

If you already have this, then pull the latest changes which will have the updated GraphQL engine Docker image.

Step 2: Attach your Heroku app
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let's say your Heroku app is called ``hasura-heroku`` and is running on ``https://hasura-heroku.herokuapp.com``.

Navigate to your project directory, use the `Heroku CLI <https://devcenter.heroku.com/articles/heroku-cli>`__ to configure the git repo you cloned in Step 1
to be able to push to this app.

.. code-block:: bash

   # Replace <hasura-heroku> with your Heroku app's name
   heroku git:remote -a <hasura-heroku>
   heroku stack:set container -a <hasura-heroku>

You can find your Heroku git repo in your Heroku - Settings - Info - Heroku Git URL

Step 3: **git push** to deploy the latest Hasura GraphQL engine
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When you ``git push`` to deploy, the Heroku app will get updated with the latest changes:

.. code-block:: bash

   git push heroku master

Deploy a specific version of the Hasura GraphQL engine
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Head to the ``Dockerfile`` in the git repo you cloned in step 1.
Change the ``FROM`` line to the specific version you want. A list of all releases can be found
at https://github.com/hasura/graphql-engine/releases.

.. code-block:: Dockerfile
   :emphasize-lines: 1

   FROM hasura/graphql-engine:v1.0.0

   ...
   ...

Change ``v1.0.0`` to ``v1.1.0`` for example, commit this and then ``git push heroku master`` to deploy.

.. note::

  If you are downgrading to an older version of the GraphQL engine you might need to downgrade your metadata catalogue version
  as described in :ref:`downgrade_hge`

Advanced
--------

- :ref:`Setting up migrations <migrations>`
