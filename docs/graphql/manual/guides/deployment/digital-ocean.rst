Run Hasura GraphQL Engine on DigitalOcean using Docker
======================================================

This guide helps you set up the Hasura GraphQL engine (w/ or w/o HTTPS) on DigitalOcean using Docker.

Step 1: Create and prepare a droplet
------------------------------------

- Login to your DigitalOcean account
- Click on ``create`` droplet then on ``One-click apps``
- Choose ``Docker`` from the ``One-click apps`` list
- Choose your droplet size
- Choose nearest datacenter region from your location
- Select your SSH key (add if you haven't added)
- Choose a hostname and then click on ``create`` button.
- Now SSH into the droplet.

Step 2: Get required setup files
--------------------------------

The `hasura/graphql-engine/install-manifests/docker-compose <https://github.com/hasura/graphql-engine/tree/master/install-manifests/docker-compose>`_ and `hasura/graphql-engine/install-manifests/docker-compose-https <https://github.com/hasura/graphql-engine/tree/master/install-manifests/docker-compose-https>`_ repos
contains all installation manifests required to deploy Hasura on DigitalOcean.

.. admonition:: Setup file download

  .. rst-class:: api_tabs
  .. tabs::

    .. tab:: HTTP

        Get the docker deployment file for HTTP installation from there:

        .. code-block:: bash

              $ wget https://raw.githubusercontent.com/hasura/graphql-engine/master/install-manifests/docker-compose/docker-compose.yaml

    .. tab:: HTTPS

        Get the docker deployment and Caddy file from there for HTTPS installation:

        .. code-block:: bash

              $ wget https://raw.githubusercontent.com/hasura/graphql-engine/master/install-manifests/docker-compose-https/docker-compose.yaml
              $ wget https://raw.githubusercontent.com/hasura/graphql-engine/master/install-manifests/docker-compose-https/Caddyfile


Step 3: Set access key to secure your GraphQL Engine
----------------------------------------------------

Edit value of ``HASURA_GRAPHQL_ACCESS_KEY`` in ``docker-compose.yaml`` and set a secure access key:

.. code-block:: bash
   :emphasize-lines: 3

   ...
    environment:
      HASURA_GRAPHQL_ACCESS_KEY: mysecretaccesskey
      HASURA_GRAPHQL_DATABASE_URL: postgres://postgres:@postgres:5432/postgres
    command:
   ...


Step 4: Set existing Postgres server (optional)
-----------------------------------------------

Edit value of ``HASURA_GRAPHQL_DATABASE_URL`` in ``docker-compose.yaml`` and set an existing ``Postgres`` server:

.. code-block:: bash
   :emphasize-lines: 4

   ...
    environment:
      HASURA_GRAPHQL_ACCESS_KEY: mysecretaccesskey
      HASURA_GRAPHQL_DATABASE_URL: postgres://user:password@hostname:port/dbname
    command:
   ...


Step 5: Set domain name for HTTPS (optional)
--------------------------------------------

Now replace ``<your-domain.com>`` in ``Caddyfile`` with your domain name.

Example: ``example.org``.

Now point domain to the public IP of the droplet in your domain DNS setting.

Step 6: Start GraphQL Engine
----------------------------

Run ``docker-compose up -d`` & then run ``docker ps`` to check if everything is running well.

It should look like this:

.. code-block:: bash

   CONTAINER ID        IMAGE                                  COMMAND                  CREATED             STATUS              PORTS                                                NAMES
  33b3f3cfa4ad        abiosoft/caddy                         "/bin/parent caddy -…"   2 hours ago         Up 2 hours          0.0.0.0:80->80/tcp, 0.0.0.0:443->443/tcp, 2015/tcp   root_caddy_1
  e2d60e13ab4e        hasura/graphql-engine:v1.0.0-alpha23   "graphql-engine serv…"   2 hours ago         Up 2 hours                                                               root_graphql-engine_1
  2c8393a72865        postgres                               "docker-entrypoint.s…"   2 hours ago         Up 2 hours          5432/tcp                                             root_postgres_1

Step 7: Open the Hasura console
-------------------------------

Head to ``https://your-domain.com/`` to open the Hasura console if you installed on HTTPS.

Head to ``http://your-droplet-ip:8080/`` to open the Hasura console if you installed on HTTP.
