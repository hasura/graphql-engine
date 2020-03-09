.. meta::
   :description: Instructions to migrate your graph.cool project to Hasura
   :keywords: hasura, docs, guide, GraphQL subscriptions, apollo, apollo-client

Migrate from graph.cool to Hasura
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

If you are looking to migrate your graph.cool project to Hasura, these are the steps that you will need to roughly follow. Do note that

Migrating Data
--------------

1. Export your Graph.cool data using their https://export.graph.cool export tool. This will give a MySQL binary dump of your current data.

.. thumbnail:: ../../../../img/graphql/manual/guides/graphcool-export.png
   :alt: Graph.cool Export

2. Use tools like pgloader to migrate from MySQL to Postgres.

Connect Hasura to Postgres
--------------------------
Once the dataset is in Postgres, connect Hasura to Postgres. Now you can rename tables/columns to match your client side queries as required. But do remember that, for every one to one relationship, Graph.cool would have created a connection table to link them. This would require a bit of work to restructure. You might also have to look at configuring enums differently in case you have them in your project. (https://hasura.io/docs/1.0/graphql/manual/schema/enums.html#enums-in-the-hasura-graphql-engine)

Restructuring connection tables
-------------------------------
// TODO


Migrating Functions
-------------------

In case you have Functions, Hasura has an equivalent feature for Event Trigger (https://hasura.io/docs/1.0/graphql/manual/event-triggers/index.html) and Custom Resolver (https://hasura.io/docs/1.0/graphql/manual/actions/index.html). Migrating this would involve taking your code and deploying it on a different platform (preferably serverless functions).


Migrating Auth
--------------

If you were using Auth0 with Graph.cool, the migration should be fairly straightforward. You can configure Hasura with Auth0 easily (https://hasura.io/docs/1.0/graphql/manual/guides/integrations/auth0-jwt.html#auth0-jwt).

Migrating Permissions
---------------------
// TODO


Related Resources
-----------------
