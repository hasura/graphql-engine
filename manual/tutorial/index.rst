.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _complete_tutorial:

The complete tutorial: Learn by example
=======================================

..
   - Install hasura CLI
   - Clone base
   - Cluster create --type=free
   - App architecture
   - API console: Users, roles & sessions
   - API console: Create tables (data modelling)
   - API console: Explore APIs as admin user, import sample data
   - API console: Permissions / access control & testing APIs
   - API console: Relationships & testing APIs
   - API console: Aggregations & views, manual relationships
   - API console: File upload
   - Add a custom login/signup API to register + add profile data
   - Add a custom API to generate stats and send email with session middleware
   - Building your UI:
     - Webapp with server-side rendering: express + mustache
        - Notes on how this can be extended to PHP or other frameworks
     - SPA (react/angular): this is only a react example
        - Use generate API code
     - react-native
        - Use generate API code
     - Android/iOS: this is only an android example
        - Use generate API code
   - Add another docker-based microservice: ghost-blog
      - Connect to postgres
      - Environment variables
   - Publish to hasura-hub

This is a simple, in-depth tutorial to understand how Hasura works and how you
can build a backend for your application, a blog engine in this example, with
Hasura.

At the end of this tutorial, you'll understand:

- what Hasura projects and clusters are,
- how to use the Hasura APIs to avoid writing backend code
- how to deploy your own microservices on a Hasura cluster

This tutorial is split across the following sections:

.. toctree::
  :maxdepth: 1

  setup-hasura-cli
  hasura-project
  hasura-cluster
  explore-hasura-cluster
  build-blog-app
  user-model
  ui-kit
  data-modelling
  explore-data-apis
  adding-relationships
  adding-permissions
  aggregations-views
  custom-api
