.. meta::
   :description: Guide for moving between environments in Hasura Cloud
   :keywords: hasura, docs, cloud, guide, local dev, staging, production, environment

.. _guide_environments_cloud:

Moving between environments
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

:ref:`This general guide <guide_environments>` discusses how to approach various stages of development with Hasura.
This page builds on that and introduces aspects that are specific to Hasura Cloud

Moving to Staging
-----------------

Running tests - regression testing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A good development workflow would require that tests be run 1) early in the development process, 
and 2) automatically with changes, to ensure changes to the schema don’t break functionality.

As you keep making schema changes, running regression tests on Hasura Cloud will ensure you are not making unwanted breaking changes.

.. note::

    Read more about :ref:`regression testing with Hasura <regression_tests>`.

Setting up CI/CD
^^^^^^^^^^^^^^^^

In addition to other commands, you may want to run ``hasura pro regression-tests run`` as part of your CI/CD workflow.

Maintenance / updates
---------------------




Updating ENV via API
^^^^^^^^^^^^^^^^^^^^

Hasura Cloud exposes GraphQL APIs to update environment variables or even create projects from scratch. 
For example, to update a few environment variables, you can make a mutation via the API like in the following example:

.. code-block:: graphql

    mutation updateTenantEnv {
      updateTenantEnv(
        tenantId: "7a79cf94-0e53-4520-a560-1b02bf522f08"
        currentHash: "6902a395d70072fbf8d36288f0eacc36c9d82e68"
        envs: [
          { key: "HASURA_GRAPHQL_ENABLE_CONSOLE", value: "false" },
          { key: "ACTIONS_ENDPOINT", value: "https://my-actions-endpoint.com/actions" }
        ]
      ) {
          hash
          envVars
        }
    }

.. note::

    Read more in the :ref:`API reference <cloud_api_reference>`.
