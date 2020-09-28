.. meta::
   :description: Deploy Hasura GraphQL engine with Docker
   :keywords: hasura, docs, deployment, docker

.. _deployment_docker:

Run Hasura GraphQL engine using Docker
======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide assumes that you already have Postgres running and helps you set up the Hasura GraphQL engine using Docker
and connect it to your Postgres database.

In case you'd like to run Hasura with a fresh Postgres database, follow :ref:`this guide <docker_simple>`
to deploy the Hasura GraphQL engine along with a Postgres instance using Docker Compose.

Stay updated with product news
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We push frequent updates to keep improving your Hasura experience. 
Sign up for our newsletter to stay updated with product news.

.. raw:: html

   <div>
      <form id="newsletter-embed-signup" name="newsletter-embed-signup" class="validate post-subscription-form mc_embed_signup newsletter-form" target="_blank" rel="noopener" novalidate>
         <div style="width: 40%">
            <div class="input-box">
               <input type="email" name="EMAIL" id="mce-EMAIL" class="mce-EMAIL" aria-label="Email" placeholder="Your Email Address" pattern="^([^\x00-\x20\x22\x28\x29\x2c\x2e\x3a-\x3c\x3e\x40\x5b-\x5d\x7f-\xff]+|\x22([^\x0d\x22\x5c\x80-\xff]|\x5c[\x00-\x7f])*\x22)(\x2e([^\x00-\x20\x22\x28\x29\x2c\x2e\x3a-\x3c\x3e\x40\x5b-\x5d\x7f-\xff]+|\x22([^\x0d\x22\x5c\x80-\xff]|\x5c[\x00-\x7f])*\x22))*\x40([^\x00-\x20\x22\x28\x29\x2c\x2e\x3a-\x3c\x3e\x40\x5b-\x5d\x7f-\xff]+|\x5b([^\x0d\x5b-\x5d\x80-\xff]|\x5c[\x00-\x7f])*\x5d)(\x2e([^\x00-\x20\x22\x28\x29\x2c\x2e\x3a-\x3c\x3e\x40\x5b-\x5d\x7f-\xff]+|\x5b([^\x0d\x5b-\x5d\x80-\xff]|\x5c[\x00-\x7f])*\x5d))*(\.\w{2,})+$" required>
            </div>
            <div id="mce-responses" class="clear display-inline mce-responses">
               <div id="mce-error-response" class="mce-error-response response error-message hide">
               </div>
               <div id="mce-success-response" class="mce-success-response response success-message hide">
               </div>
            </div>
         </div>
         <div style="position: absolute; left: -5000px;" aria-hidden="true"><input type="text" name="b_9b63e92a98ecdc99732456b0e_f5c4f66bcf" tabindex="-1" value=""></div>
         <div class="clear submit-box" style="max-width: 120px !important">
            <input type="submit" disabled="true" value="Subscribe" name="subscribe" id="mc-embedded-subscribe" class="button mc-embedded-subscribe">
         </div>
      </form>
   </div>


Deploying Hasura using Docker
-----------------------------

Prerequisites
^^^^^^^^^^^^^

- `Docker <https://docs.docker.com/install/>`_


Step 1: Get the **docker-run.sh** bash script
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The `hasura/graphql-engine/install-manifests <https://github.com/hasura/graphql-engine/tree/stable/install-manifests>`_
repo contains all installation manifests required to deploy Hasura anywhere.

Get the Docker run bash script from there:

.. code-block:: bash

   $ wget https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/docker-run/docker-run.sh

Step 2: Configure the **docker-run.sh** script
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``docker-run.sh`` script has a sample Docker run command in it. The following changes have to be
made to the command:

- Database URL
- Network config

Database URL
************

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

    You can check the :ref:`logs <docker_logs>` to see if the database credentials are proper and if Hasura is able
    to connect to the database.

  - Hasura GraphQL engine needs access permissions to your Postgres database as described in
    :ref:`Postgres permissions <postgres_permissions>`.

Network config
**************

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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Execute ``docker-run.sh`` & check if everything is running well:

.. code-block:: bash

   $ ./docker-run.sh
   $ docker ps

   CONTAINER ID  IMAGE                    ...  CREATED  STATUS  PORTS           ...
   097f58433a2b  hasura/graphql-engine..  ...  1m ago   Up 1m   8080->8080/tcp  ...

Step 4: Open the Hasura console
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Head to http://localhost:8080/console to open the Hasura console.

Step 5: Track existing tables and relationships
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

See :ref:`schema_existing_db` to enable GraphQL over the database.

.. _docker_secure:

Securing the GraphQL endpoint
-----------------------------

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to
configure an admin secret key.

Run the Docker command with an admin-secret env var
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash
   :emphasize-lines: 5

    #! /bin/bash
    docker run -d -p 8080:8080 \
     -e HASURA_GRAPHQL_DATABASE_URL=postgres://username:password@hostname:port/dbname \
     -e HASURA_GRAPHQL_ENABLE_CONSOLE=true \
     -e HASURA_GRAPHQL_ADMIN_SECRET=myadminsecretkey \
     hasura/graphql-engine:latest

.. note::

  The ``HASURA_GRAPHQL_ADMIN_SECRET`` should never be passed from the client to the Hasura GraphQL engine as it would
  give the client full admin rights to your Hasura instance. See :ref:`auth` for information on
  setting up authentication.

.. _docker_logs:

Hasura GraphQL engine server logs
---------------------------------

You can check the logs of the Hasura GraphQL engine deployed using Docker by checking the logs of the
GraphQL engine container:

.. code-block:: bash

  $ docker ps

  CONTAINER ID        IMAGE                       ...
  cdfbc6b94c70        hasura/graphql-engine..     ...

  $ docker logs cdfbc6b94c70

  {"timestamp":"2018-10-09T11:20:32.054+0000", "level":"info", "type":"http-log", "detail":{"status":200, "query_hash":"01640c6dd131826cff44308111ed40d7fbd1cbed", "http_version":"HTTP/1.1", "query_execution_time":3.0177627e-2, "request_id":null, "url":"/v1/graphql", "user":{"x-hasura-role":"admin"}, "ip":"127.0.0.1", "response_size":209329, "method":"POST", "detail":null}}
  ...

**See:**

- https://docs.docker.com/config/containers/logging for more details on logging in Docker.

- :ref:`hge_logs` for more details on Hasura logs.

.. _docker_update:

Updating Hasura GraphQL engine
------------------------------

This guide will help you update the Hasura GraphQL engine running with Docker. This guide assumes that you already have
Hasura GraphQL engine running with Docker.

Step 1: Check the latest release version
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The current latest version is:

.. raw:: html

   <code>hasura/graphql-engine:<span class="latest-release-tag">latest</span></code>

All the versions can be found at: https://github.com/hasura/graphql-engine/releases

Step 2: Update the Docker image
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the ``docker run`` command or the ``docker-compose`` command that you're running, update the image tag to this
latest version.

For example, if you had:

.. raw:: html

   <code>docker run hasura/graphql-engine:v1.0.0-alpha01 ...</code>

you should change it to:

.. raw:: html

   <code>docker run hasura/graphql-engine:<span class="latest-release-tag">latest</span> ...</code>

.. note::

  If you are downgrading to an older version of the GraphQL engine you might need to downgrade your metadata catalogue version
  as described in :ref:`downgrade_hge`

Advanced
--------

- :ref:`Setting up migrations <migrations>`

