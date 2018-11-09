Auto-apply migrations when server starts
========================================

Hasura ships a special docker container which can be used to
automatically apply migrations when the server starts:

.. code-block:: bash

   hasura/graphql-engine:<version>.cli-migrations

The ``migrations`` directory created by Hasura CLI (the one next to 
``config.yaml``) can be mounted at ``/hasura-migrations`` path of this docker
container and the container's entrypoint script will apply the migrations before
starting the server. If no directory is mounted at the designated path, server
will start ignoring migrations.

.. note::

   This container image includes Hasura CLI at ``/bin/hasura-cli`` and can be
   used for running any other CI/CD scripts in your workflow.

If you want to mount the migrations directory at some location other than
``/hasura-migrations``, set the following environment variable:

.. code-block:: bash

   HASURA_GRAPHQL_MIGRATIONS_DIR=/custom-path-for-migrations

Once the migrations are applied, the container resumes operation as a normal
Hasura GraphQL Engine server.

Example:

.. code-block:: bash

   # Start Hasura after applying the migrations present in /home/me/my-project/migrations
   docker run -p 8080:8080 \
          -v /home/me/my-project/migrations:/hasura-migrations \
          -e HASURA_GRAPHQL_DATABASE_URL=postgres://postgres:@postgres:5432/postgres \
          hasura/graphql-engine:v1.0.0-alpha27.cli-migrations
