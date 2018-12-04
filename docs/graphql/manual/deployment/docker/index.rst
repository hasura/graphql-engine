Run Hasura GraphQL Engine using Docker
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


Step 1: Get the docker run bash script
--------------------------------------

The `hasura/graphql-engine/install-manifests <https://github.com/hasura/graphql-engine/tree/master/install-manifests>`_ repo
contains all installation manifests required to deploy Hasura anywhere.

Get the docker run bash script from there:

.. code-block:: bash

   $ wget https://raw.githubusercontent.com/hasura/graphql-engine/master/install-manifests/docker-run/docker-run.sh

Step 2: Configure the docker-run.sh script
------------------------------------------

Check the sample docker run command in ``docker-run.sh``.

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

.. admonition:: Postgres on localhost

  .. rst-class:: api_tabs
  .. tabs::

    .. tab:: Linux

       If your Postgres database is running on ``localhost``, add the ``--net=host`` flag to allow the Docker container to
       access the host's network. This is what your command should look like:

       .. code-block:: bash
          :emphasize-lines: 2

          #! /bin/bash
          docker run -d --net=host \
            -e HASURA_GRAPHQL_DATABASE_URL=postgres://username:password@hostname:port/dbname \
            -e HASURA_GRAPHQL_ENABLE_CONSOLE=true \
            hasura/graphql-engine:latest

    .. tab:: Docker for Mac

       If your Postgres database is running on ``localhost``, use ``host.docker.internal`` as hostname to access
       the host's Postgres service. This is what your command should look like:

       .. code-block:: bash
          :emphasize-lines: 3

          #! /bin/bash
          docker run -d -p 8080:8080 \
            -e HASURA_GRAPHQL_DATABASE_URL=postgres://username:password@host.docker.internal:port/dbname \
            -e HASURA_GRAPHQL_ENABLE_CONSOLE=true \
            hasura/graphql-engine:latest

Step 3: Run the hasura docker container
---------------------------------------

Execute ``docker-run.sh`` & check if everything is running well:

.. code-block:: bash

   $ ./docker-run.sh
   $ docker ps

   CONTAINER ID  IMAGE                    ...  CREATED  STATUS  PORTS           ...
   097f58433a2b  hasura/graphql-engine..  ...  1m ago   Up 1m   8080->8080/tcp  ...

Step 3: Open the hasura console
-------------------------------

Head to http://localhost:8080/console to open the Hasura console.

Step 4: Track existing tables and relationships
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
