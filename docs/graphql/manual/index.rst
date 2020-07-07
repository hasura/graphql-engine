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

The Hasura GraphQL engine connects to your databases & microservices and auto-generates a production-ready GraphQL backend in minutes.

.. list-table::
   :header-rows: 1
   :class: toc-table

   * - Basics
     - Business Logic
     - Auth
   * - `Getting Started <getting-started/index>`_
     - `Actions <actions/index>`_
     - `Authentication <auth/authentication/index>`_
   * - `Schema <schema/index>`_
     - `Remote Schemas <remote-schemas/index>`_
     - `Auth Using Webhooks <auth/authentication/webhook>`_
   * - `Queries <queries/index>`_
     - `Event Triggers <event-triggers/index>`_
     - `Auth Using JWT <auth/authentication/jwt>`_
   * - `Mutations <mutations/index>`_
     - `Scheduled Triggers <scheduled-triggers/index>`_
     - `Unauthenticated / Public Access <auth/authentication/unauthenticated-access>`_
   * - `Subscriptions <subscriptions/index>`_
     - 
     - `Authorization / Access Control <auth/authorization/index>`_

.. list-table::
   :header-rows: 1
   :class: toc-table

   * - Migrate & Deploy
     - Reference
     - Learn
   * - `Basics <migrations/basics>`_
     - `Hasura CLI <hasura-cli/index>`_
     - `30-Minute Hasura Basics Course <https://hasura.io/learn/graphql/hasura/introduction/>`__
   * - `Setting Up Migrations <migrations/migrations-setup>`_
     - `API Reference <api-reference/index>`_
     - `GraphQL & Hasura Courses <https://hasura.io/learn/>`__
   * - `Managing Metadata <migrations/manage-metadata>`_
     - `How It Works <how-it-works/index>`_
     - `Guides & Resources <guides/index>`_
   * - `Deploy Using Hasura Cloud <https://hasura.io/docs/cloud/1.0/manual/index.html>`__
     - `Troubleshooting <troubleshooting/index>`_
     - 
   * - `Deploy Using Docker <deployment/docker/index>`_
     - `Security Vulnerability Protocol <security-disclosure/index>`_
     - 
   * - `Deploy Using Kubernetes <deployment/kubernetes/index>`_
     - 
     - 
    

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
