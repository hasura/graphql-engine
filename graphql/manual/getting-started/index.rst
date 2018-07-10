Getting started
===============

.. .. admonition:: What is Hasura GraphQL engine?
..  
..   Watch this `4 minute video <https://www.youtube.com/watch?time_continue=2&v=oFRwAFWXHuE>`_ to know more. If you
..   have any queries kindly ping us on chat.

To use Hasura, you need to:

#. Run the Hasura GraphQL engine with access to a Postgres database
#. Use the Hasura console (an admin UI) that connects to the Hasura GraphQL engine to help you build your schema and
   run GraphQL queries

.. image:: ../../../img/graphql/manual/getting-started/running-hasura.png
   :width: 75%
   :class: no-shadow

Choose a getting started guide:
-------------------------------

- :doc:`On Heroku <heroku-simple>` **(recommended)**: Get started in under 60 seconds with no setup required to host
  the Hasura GraphQL engine and Postgres on **Heroku's free tier**.
- :doc:`Using Docker <docker-simple>`: Run a **local development** setup that sets up the Hasura GraphQL
  engine and Postgres using Docker.

.. toctree::
   :maxdepth: 2
   :titlesonly:
   :hidden:

   Heroku quickstart <heroku-simple>
   Docker quickstart <docker-simple>
   first-graphql-query
   using-existing-database
   postgres-credentials
