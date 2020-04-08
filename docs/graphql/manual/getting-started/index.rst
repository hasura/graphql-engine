.. meta::
   :description: Get started with Hasura
   :keywords: hasura, docs, start

.. _getting_started:

Getting started
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Hasura is a GraphQL engine running on Postgres which allows you to build performant APIs easily.
It comes with :ref:`fine-grained access control <authorization>` and a comprehensive eventing system including :ref:`actions <actions>` and :ref:`remote schemas <remote_schemas>`.

The Hasura GraphQL engine consists of the following components:

.. thumbnail:: ../../../img/graphql/manual/getting-started/hasura-explained.png
   :width: 75%
   :class: no-shadow
   :alt: Running Hasura

The Hasura console allows you to configure your GraphQL API via a UI. For instance, it allows you to add tables and try out queries.
The Hasura console interacts with the Hasura GraphQL engine (server), e.g. by passing along the query.
The Hasura GraphQL engine converts incoming GraphQL queries to SQL statements and runs them against Postgres before returning the results of these to the client.

Quickstart
----------

Hasura can be deployed within a few seconds. If you want to take Hasura for a spin, get started with setting up a GraphQL API from scratch with our :ref:`Quickstart <quickstart>` guides.
You'll be able to add tables and try out queries on the Hasura console immediately, and you can explore Hasura features on the Hasura console.

Tutorials
---------

If you want to deep-dive into learning how to build a comprehensive GraphQL API including authentication and authorization,
our :ref:`GraphQL tutorials <tutorials>` are the right thing for you. 

You'll find different tutorials that include:

- `Hasura basics <https://hasura.io/learn/graphql/hasura/introduction/>`_: This tutorial will give you an overview of Hasura and its features.
- `Frontend tutorials <https://hasura.io/learn/#frontend-tutorials>`_: These tutorials will teach you how to connect Hasura with your favourite frontend frameworks.

.. toctree::
   :maxdepth: 2
   :titlesonly:
   :hidden:

   Quickstart <quickstart>
   Tutorials <tutorials>

