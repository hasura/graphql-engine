.. meta::
   :description: Guide for moving between environments in Hasura 
   :keywords: hasura, docs, guide, local dev, staging, production, environment

.. _guide_environments:

Moving between environments
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

<<<<<<< HEAD
This guide talks about the process on how to approach various stages of development with Hasura.
=======
This guide will show how to approach various stages of development with Hasura.
>>>>>>> 9c1242f88b43d2a1ee7e7b2cae8e8fa6c8369ad8

Local development
-----------------

When developing locally, there are some steps we can take to ensure a smooth transition to the next environment.

Running Hasura via docker-compose
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The simplest setup to run Hasura locally is to use the :ref:`docker-compose <get_docker_compose_file>` setup 
to run both graphql-engine and Postgres as Docker containers.

In case you have an existing Postgres setup running locally through a different environment, 
like the native Postgres Mac app or the Postgres binary installed on Linux, you can configure 
the ``HASURA_GRAPHQL_DATABASE_URL`` to point to the right Postgres connection string and remove the Postgres container from the docker-compose setup.

Migrations
^^^^^^^^^^

The state of your Postgres database is managed via incremental SQL migration files. 
These migration files can be applied one after the other to produce the final DB schema.

DB migration files can be generated incrementally and can be applied in parts to reach particular checkpoints. 
They can also be used to roll back the DB schema.

Migrations are also versioned with timestamps. At any point in time, using the migrations, 
you should be able to (re)create the schema and apply metadata to replicate the project quickly.

.. note::

    Using Hasura's migration system is optional. In case you are comfortable or familiar using other database migration tooling, 
    you can continue to use that to manage your database schema. To disable Hasura's migrations, you can do so via the Console served by the CLI. 
    Head to the ``Data`` -> ``Migrations`` tab and switch off the toggle ``Allow Postgres schema changes via console``.

Setting up a project on the Hasura CLI
**************************************

First, we need to :ref:`install the Hasura CLI <install_hasura_cli>`. 
After that, we can initialise a Hasura project by running the :ref:`hasura init <hasura_init>` command which will scaffold a project directory with migrations, metadata and config.

*New database*: If your database is clean without any existing schema, you can start using the console via the CLI (:ref:`hasura console <hasura_console>`), 
modify the Postgres schema, and the CLI will take care of creating the ``up`` and ``down`` migration files.

*Existing database*: In case you have an existing database schema, you can use the CLI to initialise the migration for that schema using the following command:

.. code-block:: bash

    hasura migrate create init-schema --from-server

This will take a ``pg_dump`` of the ``public`` schema and create an ``up`` migration to get started. 

Alternatively, if you have an SQL file with all the DDL statements, you can also specify that as an argument:

.. code-block:: bash

    hasura migrate create init-schema --sql-from-file ./file.sql

Once you set this up, you can continue to use the Hasura console served via the CLI and make any schema changes and migration files will be automatically created as you work along.

.. note::

    If you're interested in a more extensive guide on setting up migrations, see :ref:`this docs page <migrations_setup>`.

Squashing migrations
********************

During local development, we typically iterate over schema modifications multiple times. 
This leads to a large number of migration files being created over time. 
If you are building a schema for a specific feature and you really don't want to roll back or manage the smaller iterations in between, 
you can :ref:`squash the migration files into a single file <hasura_migrate_squash>` for easier organisation:

.. code-block:: bash

    hasura migrate squash --from <version>


Metadata
^^^^^^^^

The state of Hasura metadata is managed via snapshots of the metadata. Hasura stores this metadata to create the GraphQL API over Postgres and provide other functionalities like remote schemas, event triggers etc. 
All the actions performed on the console, like tracking tables/views/functions, creating relationships, configuring permissions, 
creating event triggers and remote schemas, etc. can be exported as a JSON/yaml metadata file.

These snapshots can be :ref:`exported <export_hasura_metadata>` and :ref:`imported <import_hasura_metadata>` as a whole to configure Hasura to a state represented in the snapshot.

The metadata directory of your Hasura project should be included in your version control system like git, 
so that you can roll back corresponding changes later, if required.

.. note::

    Read more about Hasura metadata on :ref:`this docs page <manage_hasura_metadata>`.

Developing and testing business logic
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Hasura allows you to write business logic in a flexible way.
If you are comfortable writing your own GraphQL server from scratch, you can add them as a :ref:`remote schema <remote_schemas>`. 
If you are interested in (re)using REST API endpoints, you can map GraphQL types with :ref:`actions <actions>`.

Adding either a remote schema or action will become part of Hasura's metadata. 
When you are adding either of them, you will have to give the HTTP handler / URL so that Hasura can communicate with that endpoint.

Assuming that the handler is also running on your local machine, 
you will need to give an endpoint that is accessible from inside the Docker container of Hasura.
Typically for Mac, this will be ``host.docker.internal`` and for Linux where the containers are running in ``host`` mode, 
it will be ``localhost``.

.. note::

    See :ref:`this docs page <docker_networking>` to learn more about Docker networking.

Configuring handlers via environment variables
**********************************************

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

Moving to Staging
-----------------

Once you are done with local dev, you may want to move to a different environment, e.g. staging.

Setting up CI/CD
^^^^^^^^^^^^^^^^

Generally, when you are done developing your app locally, you would push it to your upstream version control system like Github or Gitlab. 
You can trigger CI/CD workflows when a push is made to your upstream repository. 
When you want to deploy your changes to staging, you may push your latest code to a special branch or push a new tag which updates your staging environment.

The process for CI/CD with Hasura instances is essentially a mirror of the manual local workflow you would use. 
The CI/CD instance should download or be given the CLI as an artifact, and then run the series of commands you’d like to integrate. 
This generally includes ``hasura migrate apply`` and ``hasura metadata apply``.

To do this, you would download the CLI either through wget/curl, or if in a Dockerfile and okay with using a static version number, 
use ``COPY --from`` to extract the binary from ``hasura/graphql-engine:vX.X-cli-migrations``.

Then run the migrate/metadata/regression tests commands, passing in the endpoint and admin secret for the remote.

.. note::

    For a full CI/CD script and pre-made GitHub action, check out `this example <https://github.com/GavinRay97/hasura-ci-cd-action>`__.

Configuring environment variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are various components of Hasura metadata which are dependent on environment variables. 
This allows environment specific runtime without changing the metadata definition. 
If an environment variable is being used by some part of the metadata and isn't available in an environment, the metadata application won't succeed. 
Before applying migrations/metadata, we need to ensure that the configuration is correct. 
Additionally, you can check for the following:

- The GraphQL endpoint needs to be :ref:`secured <securing_graphql_endpoint>`. You will need to add a ``HASURA_GRAPHQL_ADMIN_SECRET`` env var.
- Environment variables for various entities like :ref:`actions <actions>` / :ref:`remote schemas <remote_schemas>` / :ref:`event triggers <event_triggers>` need to be configured.

Applying migrations and metadata
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Migrations can be :ref:`manually applied <hasura_migrate_apply>` to any Hasura instance through:

.. code-block:: bash

    hasura migrate apply --endpoint <graphql-engine-endpoint> --admin-secret <admin-secret>


This will apply only migrations which have not been already applied to the instance.

Metadata can be :ref:`manually applied <hasura_metadata_apply>` via:

.. code-block:: bash

    hasura metadata apply --endpoint <graphql-engine-endpoint> --admin-secret <admin-secret>

If you are self-hosting Hasura and have a CI/CD setup, you can also :ref:`auto-apply migrations/metadata <auto_apply_migrations>` when the graphql-engine server starts.

Going live / production
-----------------------

:ref:`Hasura Cloud <cloud_getting_started>` is the recommended hosting solution for Hasura as it takes care of Infrastructure management automatically (like auto-scaling), 
apart from providing analytics/rate limiting and other advanced features.

Like with staging, the migrations/metadata workflow needs to be repeated. Also, the following steps should be taken:

- Secure the endpoint with an admin secret.
- Disable the console - so that nobody will be able to modify schema/data directly.
- Disable APIs - except the GraphQL API, you don't need access to other APIs like pg_dump, config and metadata etc.
- Disable dev mode - you don't want expanded detailed internal error messages in production.
- Restrict CORS domains - allow only specific domains to make requests.
- Allow lists - if you know the exact GraphQL queries that would be made to the app, enable allow lists to deny any other request.

.. note::

    Read more about the above steps in the :ref:`production checklist <production_checklist>`.

Maintenance / updates
---------------------

After going live, you can continue to use the same migrations/metadata workflow via the CLI as part of incremental app building.

Updating Hasura version
^^^^^^^^^^^^^^^^^^^^^^^

Instructions on how to update the Hasura version depend on where your Hasura container is hosted. But broadly what we need to update is the Docker image 
``hasura/graphql-engine:<version>`` where the ``<version>`` will be replaced with the latest version (``v1.3.1`` for example).

Hasura Cloud is automatically updated with the most recent stable version. For other popular vendors, check out :ref:`these updating guides <update_hge>`.
