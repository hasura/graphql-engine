.. meta::
   :description: Hasura GraphQL engine documentation
   :keywords: hasura, docs, manual, graphql engine

.. title:: Hasura GraphQL Engine Documentation

Hasura GraphQL Engine Documentation
===================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Hasura GraphQL engine makes your data instantly accessible over a real-time GraphQL API, so you can build and ship modern apps and APIs faster. Hasura connects to your databases, REST servers, GraphQL servers, and third party APIs to provide a unified realtime GraphQL API across all your data sources.

.. container:: toc-list
  
  .. container:: toc-list-section

    .. container:: toc-list-head

      Basics

    .. container:: toc-list-content

      - :ref:`getting_started`
      - :ref:`schema`
      - :ref:`queries`
      - :ref:`mutations`
      - :ref:`subscriptions`

  .. container:: toc-list-section

    .. container:: toc-list-head

      Business Logic

    .. container:: toc-list-content

      - :ref:`actions`
      - :ref:`remote_schemas`
      - :ref:`event_triggers`
      - :ref:`scheduled_triggers`

  .. container:: toc-list-section

    .. container:: toc-list-head

      Auth

    .. container:: toc-list-content

      - :ref:`authentication`
      - :ref:`Auth Using Webhooks <auth_webhooks>`
      - :ref:`Auth Using JWT <auth_jwt>`
      - :ref:`Unauthenticated / Public Access <unauthenticated_access>`
      - :ref:`Authorization / Access Control <authorization>`

  .. container:: toc-list-section

    .. container:: toc-list-head

      Migrate & Deploy

    .. container:: toc-list-content

      - :ref:`Basics <migrations_basics>`
      - :ref:`Setting Up Migrations <migrations_setup>`
      - :ref:`Managing Metadata <manage_hasura_metadata>`
      - `Deploy Using Hasura Cloud <https://hasura.io/docs/cloud/1.0/manual/index.html>`__
      - :ref:`Deploy Using Docker <deployment_docker>`
      - :ref:`Deploy Using Kubernetes <deploy_kubernetes>`

  .. container:: toc-list-section

    .. container:: toc-list-head

      Reference

    .. container:: toc-list-content

      - :ref:`hasuracli_manual`
      - :ref:`api_reference`
      - :ref:`How It Works <how_it_works>`
      - :ref:`Troubleshooting <troubleshooting>`
      - :ref:`security_protocol`

  .. container:: toc-list-section

    .. container:: toc-list-head

      Learn

    .. container:: toc-list-content

      - `30-Minute Hasura Basics Course <https://hasura.io/learn/graphql/hasura/introduction/>`__
      - `GraphQL & Hasura Courses <https://hasura.io/learn/>`__
      - :ref:`Guides & Resources <guides>`


.. toctree::
  :maxdepth: 1
  :titlesonly:
  :hidden:

  getting-started/index
  schema/index
  queries/index
  mutations/index
  subscriptions/index
  actions/index
  remote-schemas/index
  event-triggers/index
  scheduled-triggers/index
  auth/index
  migrations/index
  Deploying <deployment/index>
  hasura-cli/index
  API Reference <api-reference/index>
  How It Works <how-it-works/index>
  Troubleshooting <troubleshooting/index>
  guides/index
  security-disclosure/index
