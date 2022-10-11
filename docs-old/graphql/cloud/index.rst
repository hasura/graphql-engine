.. meta::
   :description: Hasura Cloud documentation
   :keywords: hasura, docs, manual, graphql engine, cloud, hosted

.. title:: Hasura Cloud Documentation

.. _cloud_docs:

Hasura Cloud Documentation
==========================

`Hasura Cloud <https://cloud.hasura.io/signup/?pg=docs&plcmt=body&cta=hasura-cloud&tech=default>`__ offers hosted `GraphQL Engine <https://github.com/hasura/graphql-engine>`__
projects with extra features for reliability and security. It includes all the :ref:`core features <core_docs>`
of GraphQL Engine, while taking care of infrastructure concerns, such as the number of instances, cores, memory, concurrent users, high-availability,
realtime monitoring, caching, tracing, and rate-limiting. It supports both new and existing databases.

.. container:: toc-list

  .. container:: toc-list-section

    .. container:: toc-list-head

      Basics

    .. container:: toc-list-content

      - :ref:`cloud_getting_started`
      - :ref:`create_project`
      - :ref:`manage_project_collaborators`
      - :ref:`manage_project_env_vars`
      - :ref:`manage_project_domains`

  .. container:: toc-list-section

    .. container:: toc-list-head

      Features

    .. container:: toc-list-content

      - :ref:`metrics`
      - :ref:`regression_tests`
      - :ref:`read_replicas`
      - :ref:`response_caching`
      - :ref:`tracing`
      - :ref:`query_tags`
      - :ref:`dedicated_vpc`

  .. container:: toc-list-section

    .. container:: toc-list-head

       API Security

    .. container:: toc-list-content

       - :ref:`api_limits`
       - :ref:`allow_lists`

  .. container:: toc-list-section

    .. container:: toc-list-head

      Reference

    .. container:: toc-list-content

      - :ref:`cloud_api_reference`
      - :ref:`glossary`
      - :ref:`hasurapro_cli`

.. toctree::
  :maxdepth: 1
  :titlesonly:
  :hidden:

  Getting Started <getting-started/index>
  projects/index
  metrics/index
  API security <security/index>
  regression-tests
  read-replicas
  response-caching
  tracing
  query-tags
  Preview apps <preview-apps>
  dedicated-vpc
  Billing <billing/index>
  Account management <account-management/index>
  hasurapro-cli/index
  api-reference
  glossary
