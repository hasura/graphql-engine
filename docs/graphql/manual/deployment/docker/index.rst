Run Hasura GraphQL engine using Docker
======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide assumes that you already have Postgres running and helps you set up the Hasura GraphQL engine using Docker
and connect it to your Postgres database.

Prerequisites
-------------

- `Docker <https://docs.docker.com/install/>`_


Step 1: Get the **docker-run.sh** bash script
---------------------------------------------

The `hasura/graphql-engine/install-manifests <https://github.com/hasura/graphql-engine/tree/master/install-manifests>`_
repo contains all installation manifests required to deploy Hasura anywhere.

Get the Docker run bash script from there:

.. code-block:: bash

   $ wget https://raw.githubusercontent.com/hasura/graphql-engine/master/install-manifests/docker-run/docker-run.sh

Step 2: Configure the **docker-run.sh** script
----------------------------------------------

The ``docker-run.sh`` script has a sample Docker run command in it. The following changes have to be
made to the command:

- Database URL
- Network config

Database URL
^^^^^^^^^^^^

Edit the ``HASURA_GRAPHQL_DATABASE_URL`` env var value, so that you can connect to your Postgres instance.

.. code-block:: bash
   :emphasize-lines: 3

   #! /bin/bash
   docker run -d -p 8080:8080 \
     -e HASURA_GRAPHQL_DATABASE_URL=postgres://username:password@hostname:port/dbname \
     -e HASURA_GRAPHQL_ENABLE_CONSOLE=true \
     hasura/graphql-engine:latest

Examples of ``HASURA_GRAPHQL_DATABASE_URL``:

- ``postgres://admin:password@localhost:5432/my-db``
- ``postgres://admin:@localhost:5432/my-db`` *(if there is no password)*

.. note::

  - If your **password contains special characters** (e.g. #, %, $, @, etc.), you need to URL encode them in the
    ``HASURA_GRAPHQL_DATABASE_URL`` env var (e.g. %40 for @).

    You can check the :doc:`logs <logging>` to see if the database credentials are proper and if Hasura is able
    to connect to the database.

  - Hasura GraphQL engine needs access permissions to your Postgres database as described in
    :doc:`Postgres permissions <../postgres-permissions>`.

Network config
^^^^^^^^^^^^^^

If your Postgres instance is running on ``localhost``, the following changes will be needed to the ``docker run``
command to allow the Docker container to access the host's network:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Linux

     Add the ``--net=host`` flag to access the host's Postgres service.

     This is what your command should look like:

     .. code-block:: bash
        :emphasize-lines: 1

        docker run -d --net=host \
          -e HASURA_GRAPHQL_DATABASE_URL=postgres://username:password@hostname:port/dbname \
          -e HASURA_GRAPHQL_ENABLE_CONSOLE=true \
          hasura/graphql-engine:latest

  .. tab:: Docker for Mac

     Use ``host.docker.internal`` as hostname to access the host's Postgres service.

     This is what your command should look like:

     .. code-block:: bash
        :emphasize-lines: 2

        docker run -d -p 8080:8080 \
          -e HASURA_GRAPHQL_DATABASE_URL=postgres://username:password@host.docker.internal:port/dbname \
          -e HASURA_GRAPHQL_ENABLE_CONSOLE=true \
          hasura/graphql-engine:latest

  .. tab:: Docker for Windows

     Use ``docker.for.win.localhost`` as hostname to access the host's Postgres service.

     This is what your command should look like:

     .. code-block:: bash
        :emphasize-lines: 2

        docker run -d -p 8080:8080 \
          -e HASURA_GRAPHQL_DATABASE_URL=postgres://username:password@docker.for.win.localhost:port/dbname \
          -e HASURA_GRAPHQL_ENABLE_CONSOLE=true \
          hasura/graphql-engine:latest
          

Step 3: Run the Hasura Docker container
---------------------------------------

Execute ``docker-run.sh`` & check if everything is running well:

.. code-block:: bash

   $ ./docker-run.sh
   $ docker ps

   CONTAINER ID  IMAGE                    ...  CREATED  STATUS  PORTS           ...
   097f58433a2b  hasura/graphql-engine..  ...  1m ago   Up 1m   8080->8080/tcp  ...

Step 4: Open the Hasura console
-------------------------------

Head to http://localhost:8080/console to open the Hasura console.

Step 5: Track existing tables and relationships
-----------------------------------------------

See :doc:`../../schema/using-existing-database` to enable GraphQL over the database.

Advanced
--------

- :doc:`Securing your GraphQL endpoint <securing-graphql-endpoint>`
- :doc:`GraphQL engine server logs <logging>`
- :doc:`Updating GraphQL engine <updating>`
- :doc:`Setting up migrations <../../migrations/index>`

.. toctree::
   :titlesonly:
   :hidden:

   Securing your GraphQL endpoint <securing-graphql-endpoint>
   GraphQL engine server logs <logging>
   Updating GraphQL engine <updating>
