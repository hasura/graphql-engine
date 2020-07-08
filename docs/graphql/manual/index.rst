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

      - `Getting Started <getting-started/index>`_
      - `Schema <schema/index>`_
      - `Queries <queries/index>`_
      - `Mutations <mutations/index>`_
      - `Subscriptions <subscriptions/index>`_

  .. container:: toc-list-section

    .. container:: toc-list-head

      Business Logic

    .. container:: toc-list-content

      - `Actions <actions/index>`_
      - `Remote Schemas <remote-schemas/index>`_
      - `Event Triggers <event-triggers/index>`_
      - `Scheduled Triggers <scheduled-triggers/index>`_

  .. container:: toc-list-section

    .. container:: toc-list-head

      Auth

    .. container:: toc-list-content

      - `Authentication <auth/authentication/index>`_
      - `Auth Using Webhooks <auth/authentication/webhook>`_
      - `Auth Using JWT <auth/authentication/jwt>`_
      - `Unauthenticated / Public Access <auth/authentication/unauthenticated-access>`_
      - `Authorization / Access Control <auth/authorization/index>`_

  .. container:: toc-list-section

    .. container:: toc-list-head

      Migrate & Deploy

    .. container:: toc-list-content

      - `Basics <migrations/basics>`_
      - `Setting Up Migrations <migrations/migrations-setup>`_
      - `Managing Metadata <migrations/manage-metadata>`_
      - `Deploy Using Hasura Cloud <https://hasura.io/docs/cloud/1.0/manual/index.html>`__
      - `Deploy Using Docker <deployment/docker/index>`_
      - `Deploy Using Kubernetes <deployment/kubernetes/index>`_

  .. container:: toc-list-section

    .. container:: toc-list-head

      Reference

    .. container:: toc-list-content

      - `Hasura CLI <hasura-cli/index>`_
      - `API Reference <api-reference/index>`_
      - `How It Works <how-it-works/index>`_
      - `Troubleshooting <troubleshooting/index>`_
      - `Security Vulnerability Protocol <security-disclosure/index>`_

  .. container:: toc-list-section

    .. container:: toc-list-head

      Learn

    .. container:: toc-list-content

      - `30-Minute Hasura Basics Course <https://hasura.io/learn/graphql/hasura/introduction/>`__
      - `GraphQL & Hasura Courses <https://hasura.io/learn/>`__
      - `Guides & Resources <guides/index>`_


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
