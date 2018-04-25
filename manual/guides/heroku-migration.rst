Migrating from Heroku to Hasura
===============================

This guide is for migrating your existing Heroku app to Hasura.

The reason for moving from Heroku to Hasura is beyond the scope of this guide,
you must have your reasons.

The high-level steps involve:

1. Move your Postgres data from Heroku to Hasura.
2. Migrate your Heroku apps to Hasura microservices.
3. Customize your new Hasura microservices to use any add-on that you were using
   on Heroku (if needed).


Migrating data
--------------
If you have data in Heroku's Postgres database, you need to move them to
Hasura's Postgres. Follow this :doc:`guide
<../guides/importing-existing-database>` to migrate data from any Postgres
instance to Hasura's Postgres.

If you have any other database on Heroku (like MongoDB via add-ons), then there
are two options:

1. You keep running the add-on on Heroku and configure your Hasura microservices
   to access them. See :ref:`heroku-customize-microservice`.
2. You create a custom database microservice on Hasura (You need Hasura's pro-tier
   clusters for this). See :ref:`heroku-stateful-microservice`.

Migrating Heroku apps to microservices
--------------------------------------
What Heroku calls ``app``, Hasura calls ``microservice``. Roughly though. Heroku also
calls the server/cluster as an app. Unlike Heroku, Hasura makes a distinction
between a cluster (a server running Hasura software), a microservice (your
source code for a particular service), a Hasura project (collection of
microservices, and your schema).

In a Heroku app you would need a ``Procfile`` for your app, for a Hasura
microservice you would need a ``Dockerfile`` and a ``k8s.yaml`` file. In Hasura
everything runs as a Docker container on a Kubernetes cluster.

You can find readymade Hasura projects and microservices to use as templates
for most of the popular stacks on https://hasura.io/hub

:doc:`Read more <../microservices/add-microservice/index>` on how to create
custom microservices.

.. _heroku-customize-microservice:

Customizing your Hasura microservices to use Heroku add-ons
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
If you are using any Heroku add-on, you can customize your microservices by
adding environment variables, adding secret info like API keys etc.

You can add your add-on's URL, and other secret information as environment
variables and secrets (like API keys, passwords) into your microservice, and
then access them in your code.

:doc:`Read more <../microservices/env-variables>` on how to pass ENV variables
to your microservices.

Once configured, you can continue accessing your existing Heroku add-ons from
inside the microservices running on Hasura.

.. _heroku-stateful-microservice:

Running stateful microservices on Hasura
++++++++++++++++++++++++++++++++++++++++
Hasura microservices are generally stateless. You can configure any microservice to
use a persistent volume and make it stateful. For example if you use mongodb or redis etc.

.. note::

   This feature is available only on pro-tier clusters. 

See :doc:`this guide <../microservices/persistent-volumes>` to configure your
microservice to be stateful.
