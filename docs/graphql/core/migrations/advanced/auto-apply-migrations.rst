.. meta::
   :description: Auto-apply migrations and metadata when the server starts
   :keywords: hasura, docs, auto-apply, migration, metadata, seeds, server

.. _auto_apply_migrations:

Auto-apply migrations/metadata/seeds when the server starts
=====================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

**cli-migrations** image
------------------------

Hasura ships a special Docker image which can be used to
automatically apply migrations/metadata/seeds when the server starts:

.. code-block:: bash

   hasura/graphql-engine:<version>.cli-migrations-v3

This container image includes the Hasura CLI at ``/bin/hasura-cli`` and can be
used for running any other CI/CD scripts in your workflow.

.. note::

  For ``config v2``, see :ref:`auto_apply_migrations_v2`.

Applying migrations
-------------------

The ``migrations``, ``metadata``, and ``seeds`` directories created by the Hasura CLI in a
Hasura project can be mounted at the ``/hasura-migrations``,  ``/hasura-metadata`` and ``/hasura-seeds``
path respectively of this Docker container and the container's entry point script will apply the
migrations, metadata and seeds before starting the server. If no directory is mounted at
the designated paths, the server will start ignoring the migrations and/or metadata and/or seeds.

If you want to mount the migrations/metadata/seeds directories at some location other
than the above, set the following environment variables:

.. code-block:: bash

   HASURA_GRAPHQL_MIGRATIONS_DIR=/custom-path-for-migrations
   HASURA_GRAPHQL_METADATA_DIR=/custom-path-for-metadata
   HASURA_GRAPHQL_SEEDS_DIR=/custom-path-for-seeds

Once the migrations, metadata and seeds are applied, the container resumes operation as
a normal Hasura GraphQL engine server.

Example:

.. code-block:: bash

   # Start Hasura after applying the migrations and metadata present in the Hasura project
   docker run -p 8080:8080 \
          -v /home/me/my-project/migrations:/hasura-migrations \
          -v /home/me/my-project/metadata:/hasura-metadata \
          -v /home/me/my-project/seeds:/hasura-seeds \
          -e HASURA_GRAPHQL_DATABASE_URL=postgres://postgres:@postgres:5432/postgres \
          hasura/graphql-engine:<version>.cli-migrations-v3


.. _auto_apply_metadata:

Applying only metadata
----------------------

If you're managing migrations with a different tool and want to use this image
to apply only the metadata, mount the ``metadata`` directory of your Hasura project
at the ``/hasura-metadata`` path of this Docker container the container’s entry point
script will apply the metadata before starting the server.

