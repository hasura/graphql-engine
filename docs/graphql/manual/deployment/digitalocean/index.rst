Run Hasura GraphQL Engine using Docker on DigitalOcean
======================================================

This guide helps you set up the Hasura GraphQL engine using Docker on DigitalOcean.

**Prerequisites**:

- No Special requirement, just putty if you are on windows

Step 1: Create and prepare a droplet
------------------------------------

- Login to your DigitalOcean account
- Click on create droplet then on ``One-click apps``
- Choose ``Docker`` from the ``One-click apps`` list
- Choose your droplet size
- Choose nearest datacenter region from your location
- Select your ssh key add if you haven't added
- Choose a hostname and then click on create button.
- Now SSH into the droplet.
- Run ``apt install update && apt upgrade -y`` to update system 

Step 2: Get the Docker deployment and Caddy file 
------------------------------------------------

The `hasura/graphql-engine/install-manifests/docker-compose-https <https://github.com/hasura/graphql-engine/tree/master/install-manifests/docker-compose-https>`_ repo
contains all installation manifests required to deploy Hasura on DigitalOcean.

Get the docker deployment and Caddy file from there:

.. code-block:: bash

   $ wget https://raw.githubusercontent.com/hasura/graphql-engine/master/install-manifests/docker-compose-https/docker-compose.yaml
   $ wget https://raw.githubusercontent.com/hasura/graphql-engine/master/install-manifests/docker-compose-https/Caddyfile

Step 3: Set an access key and point domain to the public ip of the droplet. 
---------------------------------------------------------------------------

Edit value of ``HASURA_GRAPHQL_ACCESS_KEY`` in ``docker-compose.yaml`` and set a secure access key:


.. code-block:: bash
   :emphasize-lines: 3

   ...
    environment:
      HASURA_GRAPHQL_ACCESS_KEY: mysecretaccesskey
      HASURA_GRAPHQL_DATABASE_URL: postgres://postgres:@postgres:5432/postgres
    command:
  ...

Now replace ``<your-domain.com>`` in ``Caddyfile`` with your domain name 

Example ``example.org``


Step 4: Run the hasura docker container
---------------------------------------

Run ``docker-compose up -d`` & check if everything is running well:

.. code-block:: bash

   $ docker ps

   CONTAINER ID        IMAGE                                  COMMAND                  CREATED             STATUS              PORTS                                                NAMES
  33b3f3cfa4ad        abiosoft/caddy                         "/bin/parent caddy -…"   2 hours ago         Up 2 hours          0.0.0.0:80->80/tcp, 0.0.0.0:443->443/tcp, 2015/tcp   root_caddy_1
  e2d60e13ab4e        hasura/graphql-engine:v1.0.0-alpha23   "graphql-engine serv…"   2 hours ago         Up 2 hours                                                               root_graphql-engine_1
  2c8393a72865        postgres                               "docker-entrypoint.s…"   2 hours ago         Up 2 hours          5432/tcp                                             root_postgres_1

Step 5: Open the hasura console
-------------------------------

Head to https://your-domain.com/console to open the Hasura console.

and you are done.