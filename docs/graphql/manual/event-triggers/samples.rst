.. meta::
   :description: Examples for event triggers with Hasura
   :keywords: hasura, docs, event trigger, example

.. _trigger_samples:

Event trigger samples
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


Boilerplates
^^^^^^^^^^^^

Here are a few boilerplates you can use to build and deploy event triggers on different cloud providers:

* Source code: https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/event-triggers

There are 2 types of boilerplates:

**Echo**
Returns the event payload with some augmented data. It helps you in understanding the event payload and parsing it.

**Mutation**
Makes a mutation based on the event payload. It helps in understanding database access inside an event trigger.

Push Notifications
^^^^^^^^^^^^^^^^^^

Here's a `notification demo app <https://serverless-push.demo.hasura.app/>`_ showcasing sending web
notifications using Hasura event triggers and FCM.

* Video: https://www.youtube.com/watch?v=nuSHkzE2-zo&feature=youtu.be
* Source code: https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/serverless-push

Data Transformations (ETL)
^^^^^^^^^^^^^^^^^^^^^^^^^^
Here's a `serverless ETL demo app <https://serverless-etl.demo.hasura.app/>`_ built using Hasura event triggers and
Algolia search.

* Video: https://youtu.be/kWVEBWdEVAA
* Source code: https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/serverless-etl
