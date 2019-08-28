Quickstart with Nhost
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


Page for Nhost free trail.

- PostgreSQL
- Hasura GraphQL Engline
- Authentication
- Storage

This guide will help you get Hasura GraphQL engine and Postgres running on `Nhost's 30 days free trail <https://nhost.io/pricing#free>`_.
It is the easiest and fastest way of trying Hasura out.

If you'd like to link this to an existing database, please head to this guide instead:
:doc:`Using an existing database on Heroku <../deployment/heroku/using-existing-heroku-database>`.

Deploy to Nhost
----------------

Sign up on Nhost and create a project: `Sign up <https://app.nhost.io/register>`_.

.. note::
   Sign up is free. No credit card required.

.. thumbnail:: ../../../img/graphql/manual/getting-started/nhost-create-project.png

Open the Hasura console
-----------------------

That's it!  Click on the Hasura Console URL and use your admin secret (`HASURA_GRAPHQL_ADMIN_SECERT`) to login.
You should now see the Hasura console.

.. thumbnail:: ../../../img/graphql/manual/getting-started/nhost-go-to-hasura-console.png

Hello World (GraphQL or event triggers)
---------------------------------------

Make your :doc:`first graphql query <first-graphql-query>`

OR

Set up your :doc:`first event trigger <first-event-trigger>`

Advanced
--------

This was a quickstart guide to get the Hasura GraphQL engine up and running quickly. For more detailed instructions
on deploying using Nhost, check out :doc:`../deployment/nhost/index`.
