Getting started
===============

.. .. admonition:: What is Hasura GraphQL engine?
..  
..   Watch this `4 minute video <https://www.youtube.com/watch?time_continue=2&v=oFRwAFWXHuE>`_ to know more. If you
..   have any queries kindly ping us on chat.

To use Hasura, you need to:

#. Run the Hasura GraphQL engine that has access to a postgres database
#. Use the Hasura console (an admin UI) that connects to the Hasura GraphQL engine to help you build your schema and run GraphQL queries

.. image:: ../../../img/graphql/manual/getting-started/running-hasura.png
   :width: 50%
   :class: no-shadow

Choose a getting started guide:
-------------------------------

- :doc:`Heroku free tier <heroku-free>` **(recommended)**: Get started in under 60s with no setup required to host the Hasura GraphQL engine or postgres.
- :doc:`Local development <docker-compose>`: Run a local development setup with docker that sets up Hasura and Postgres.

Other guides:
-------------

- :doc:`Heroku advanced <heroku>`: A guide to hosting Hasura GraphQL engine on Heroku with your Heroku postgres
- :doc:`Docker <docker>`: A guide to running Hasura GraphQL engine as a docker container connected to your Postgres database

.. toctree::
   :maxdepth: 2
   :titlesonly:
   :hidden:

   Setup on Heroku free tier <heroku-free>
   Setup for local development <docker-compose>
   Setup on Heroku (advanced) <heroku>
   Setup with docker <docker>
   first-graphql-query
   using-existing-database
   postgres-credentials
