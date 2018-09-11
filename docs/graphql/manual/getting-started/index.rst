Getting started
===============

To use the Hasura GraphQL engine, you need to:

#. Run the Hasura GraphQL engine with access to a Postgres database
#. Use the Hasura console (an admin UI) that connects to the Hasura GraphQL engine to help you build your schema and
   run GraphQL queries

.. image:: ../../../img/graphql/manual/getting-started/running-hasura.png
   :width: 75%
   :class: no-shadow

Choose a getting started guide:
-------------------------------

- :doc:`Using Heroku <heroku-simple>` **(recommended)**: Get started in under 60 seconds with no setup required to
  host the Hasura GraphQL engine and Postgres on Heroku's free tier.
- :doc:`Using Docker <docker-simple>`: Run a **local development** setup that sets up both the Hasura GraphQL
  engine and Postgres using Docker.

Using an existing database:
^^^^^^^^^^^^^^^^^^^^^^^^^^^
- :doc:`Using Heroku <../deployment/heroku/using-existing-heroku-database>`: Run on Heroku using an existing Heroku
  Postgres database.
- :doc:`Using Docker <../deployment/docker/index>`: Run as a docker container and connect to an existing Postgres
  database.
- :doc:`Using Kubernetes <../deployment/kubernetes/index>`: Run on Kubernetes and connect to an existing Postgres
  database.

.. toctree::
   :maxdepth: 2
   :titlesonly:
   :hidden:

   Heroku quickstart <heroku-simple>
   Docker quickstart <docker-simple>
   Using an existing database <using-existing-database>
   first-graphql-query
   first-event-trigger
