.. meta::
   :description: Guide for managing development environments for Hasura Cloud
   :keywords: hasura, docs, cloud, guide, local dev, staging, production, environment

.. _guide_environments_cloud:

Managing development environments for Hasura Cloud
==================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This guide will show how to configure your Hasura project to make it easy to switch to other development environments.

Best practices
--------------

In the following, we'll go over some best practices for the project setup that will help ensuring a smooth transition of environments later.

.. _env_guide_migrations_metadata:

Migrations & metadata
^^^^^^^^^^^^^^^^^^^^^

The state of your Postgres database is managed via incremental SQL migration files.
These migration files can be applied one after the other to produce the final DB schema.

.. note::

    Using Hasura's migration system is optional. In case you are comfortable or familiar using other database migration tooling, 
    you can continue to use that to manage your database schema.

The state of Hasura metadata is managed via snapshots of the metadata. Hasura stores this metadata to create the GraphQL API over Postgres and provide other functionalities like remote schemas, event triggers etc.

Migrations & metadata allow you to move between development environments smootly. At any point in time, using the migrations, 
you should be able to (re)create the schema and apply metadata to replicate the project quickly. 

.. note::

  Read more about :ref:`migrations <migrations_setup>` and :ref:`metadata <manage_hasura_metadata>`.

Developing & testing business logic
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Hasura lets you write business logic in a flexible way. There are several options depending on your use case:

- If you are comfortable writing your own GraphQL server from scratch, you can add them as a :ref:`remote schema <remote_schemas>`. 
- If you are interested in (re)using REST API endpoints, you can map GraphQL types with :ref:`actions <actions>`.
- If you want to trigger a serverless function based on a database event, you can use :ref:`event triggers <event_triggers>`. 

Configuring handlers via environment variables
**********************************************

Adding any of the above business logic options will become part of Hasura's metadata. 
When you are adding either of them, you will have to give the HTTP handler / URL so that Hasura can communicate with that endpoint.

.. note::

    See :ref:`this docs page <docker_networking>` to learn about local development and Docker networking.

The recommended way to configure these handler URLs is via environment variables, 
irrespective of the development environment (local/staging/prod).

**Actions**

Actions can have a base URL through ENVs, something like ``{{ACTION_BASE_URL}}``. 
For example, if all your REST API endpoints are running in a single server, you can configure the ENV with the host name.

.. code-block:: bash

  {{ACTION_BASE_URL}}/createUser

The ``{{ACTION_BASE_URL}}`` will typically have values like ``http://myserver.com`` or when running in localhost, 
it will look something like ``http://localhost:3000``. All of this will be passed to the graphql-engine server as ENVs.

Forwarding headers can be configured using ENV. 
This will be applicable when you are sharing a common secret between your action handler or when passing some authorization tokens etc.

.. note::

    Read more in the :ref:`action documentation <actions>`.

**Remote schemas**

The GraphQL server URL can come from an env var. Similarly, any additional headers can be configured that can have values from ENV.

.. note::

    Read more in the :ref:`remote schema documentation <adding_schema>`.

**Event triggers**

When creating event triggers, you can specify the URL for the event handler via ENV.

.. note::

    Read more in the :ref:`event trigger documentation <create_trigger>`.

Debugging in local development
******************************

During local development, you may want to look at errors in detail, so that you can fix the underlying issue. 
For a GraphQL request that results in an error, Hasura may provide additional information for each object in the ``extensions`` key of ``errors``. 
The ``internal`` key contains error information including the generated SQL statement and exception information from Postgres. 
This can be highly useful, especially in the case of debugging errors in action requests.

:ref:`Enable the dev mode debugging <dev-mode>` via the ``HASURA_GRAPHQL_DEV_MODE`` environment variable.

Configuring environment variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are various components of Hasura metadata which are dependent on environment variables. 
This allows environment specific runtime without changing the metadata definition. 
If an environment variable is being used by some part of the metadata and isn't available in an environment, the metadata application won't succeed. 
Before applying :ref:`migrations/metadata <env_guide_migrations_metadata>` to a new environment, we need to ensure that the configuration is correct. 
Additionally, you can check for the following:

- The GraphQL endpoint needs to be :ref:`secured <securing_graphql_endpoint>`. You will need to add an ``HASURA_GRAPHQL_ADMIN_SECRET`` env var.
- Environment variables for various entities like :ref:`actions <actions>` / :ref:`remote schemas <remote_schemas>` / :ref:`event triggers <event_triggers>` need to be configured.

Running tests - regression testing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A good development workflow would require that tests be run 1) early in the development process, 
and 2) automatically with changes, to ensure changes to the schema don’t break functionality.

As you keep making schema changes, running regression tests on Hasura Cloud will ensure you are not making unwanted breaking changes.

.. note::

    Read more about :ref:`regression testing with Hasura <regression_tests>`.

Setting up CI/CD
^^^^^^^^^^^^^^^^

Generally, when developing an app, you would push it to your upstream version control system like Github or Gitlab. 
You can trigger CI/CD workflows when a push is made to your upstream repository. 
When you want to deploy your changes to staging, you may push your latest code to a special branch or push a new tag which updates your staging environment.

The process for CI/CD with Hasura instances is essentially a mirror of the manual local workflow you would use. 
The CI/CD instance should download or be given the CLI as an artifact, and then run the series of commands you’d like to integrate. 
This generally includes ``hasura migrate apply`` and ``hasura metadata apply``, and might also include ``hasura pro regression-tests run``.

To do this, you would download the CLI either through wget/curl, or if in a Dockerfile and okay with using a static version number, 
use ``COPY --from`` to extract the binary from ``hasura/graphql-engine:vX.X-cli-migrations``.

Then run the migrate/metadata/regression tests commands, passing in the endpoint and admin secret for the remote.

.. note::

    For a full CI/CD script and pre-made GitHub action, check out `this example <https://github.com/GavinRay97/hasura-ci-cd-action>`__.

Recommended setups
------------------

The recommended setup differs depending on your use case and requirements. 
In the following, we'll provide you with some recommendations for the development environment and production environment.

Development environment
^^^^^^^^^^^^^^^^^^^^^^^

The recommended development environment setup depends on if you want to host Hasura in the cloud or locally.

Cloud development: Hasura Cloud
*******************************

:ref:`Hasura Cloud <cloud_getting_started>` is our recommended deployment option if you're use case allows hosting your project in the cloud. 
It will will allow you to use features that will help you in collaboration, monitoring, as well as in controlling requests to your API.
These include:

- :ref:`Managing collaborators <manage_project_collaborators>`
- :ref:`Metrics <metrics>`
- :ref:`Allos lists <allow_lists>`
- :ref:`API limits <api_limits>`

Local development: Docker
*************************

The simplest setup to run Hasura locally is to use the :ref:`docker-compose <get_docker_compose_file>` setup 
to run both graphql-engine and Postgres as Docker containers.

In case you have an existing Postgres setup running locally through a different environment, 
like the native Postgres Mac app or the Postgres binary installed on Linux, you can configure 
the ``HASURA_GRAPHQL_DATABASE_URL`` to point to the right Postgres connection string and remove the Postgres container from the docker-compose setup.

Production environment
^^^^^^^^^^^^^^^^^^^^^^

When moving to production, it's important to keep in mind the points below. Some of them you might already have configured with the project setup.

- Secure the endpoint with an admin secret.
- Disable the console - so that nobody will be able to modify schema/data directly.
- Disable APIs - except the GraphQL API, you don't need access to other APIs like pg_dump, config and metadata etc.
- Disable dev mode - you don't want expanded detailed internal error messages in production.
- Restrict CORS domains - allow only specific domains to make requests.
- Allow lists - if you know the exact GraphQL queries that would be made to the app, enable allow lists to deny any other request.

.. note::

  Read more about the above steps in the :ref:`production checklist <production_checklist>`.

Maintenance / updates
^^^^^^^^^^^^^^^^^^^^^

Updating Hasura version
***********************

Hasura Cloud is automatically updated with the most recent stable version. 

.. note::

  In the future, it will be possible to downgrade to an earlier version, as well as upgrade to beta versions.

Updating ENV via API
********************

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
