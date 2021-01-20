.. meta::
   :description: Auto-apply migrations and metadata when the server starts
   :keywords: hasura, docs, auto-apply, migration, metadata, server

.. _auto_apply_migrations_v1:

Auto-apply migrations/metadata when the server starts (config v1)
=================================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Hasura ships a special Docker container which can be used to
automatically apply migrations/metadata when the server starts:

.. code-block:: bash

   hasura/graphql-engine:<version>.cli-migrations

.. note::

   This container image includes the Hasura CLI at ``/bin/hasura-cli`` and can be
   used for running any other CI/CD scripts in your workflow.

Applying migrations
-------------------

The ``migrations`` directory created by the Hasura CLI (the one next to 
``config.yaml``) can be mounted at the ``/hasura-migrations`` path of this Docker
container and the container's entry point script will apply the migrations before
starting the server. If no directory is mounted at the designated path, the server
will start ignoring migrations.

If you want to mount the migrations directory at some location other than
``/hasura-migrations``, set the following environment variable:

.. code-block:: bash

   HASURA_GRAPHQL_MIGRATIONS_DIR=/custom-path-for-migrations

Once the migrations are applied, the container resumes operation as a normal
Hasura GraphQL engine server.

Example:

.. code-block:: bash

   # Start Hasura after applying the migrations present in /home/me/my-project/migrations
   docker run -p 8080:8080 \
          -v /home/me/my-project/migrations:/hasura-migrations \
          -e HASURA_GRAPHQL_DATABASE_URL=postgres://postgres:@postgres:5432/postgres \
          hasura/graphql-engine:v1.1.0.cli-migrations


.. _auto_apply_metadata_v1:

Applying only metadata
----------------------

If you're managing migrations with a different tool and want to use this image to apply only the
metadata, mount a directory with just a ``metadata.yaml`` file and the script will
apply the metadata.
