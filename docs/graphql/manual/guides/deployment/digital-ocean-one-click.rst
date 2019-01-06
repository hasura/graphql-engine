Hasura GraphQL Engine DigitalOcean One-click App
================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Hasura GraphQL Engine is available as a One-click app on DigitalOcean. It is
packed with a Postgres database and `Caddy <https://caddyserver.com/>`__
webserver for easy and automatic HTTPS using Let's Encrypt.


Quickstart
----------

1. Create a Hasura One-click Droplet
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Click the button below to create a new Hasura GraphQL Engine Droplet on
DigitalOcean using One-Click App. (``Ctrl+Click`` to open in a new tab)

.. image:: https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet.png
   :width: 300px
   :alt: do_create_droplet_button
   :class: no-shadow
   :target: https://cloud.digitalocean.com/droplets/new?image=hasura-18-04&utm_source=hasura&utm_campaign=docs

2. Open console
~~~~~~~~~~~~~~~

Once Hasura GraphQL Engine One-Click Droplet is ready, you can visit the Droplet
IP to open the Hasura console, where you can create tables, explore GraphQL APIs
etc. Note that it might take 1 or 2 minutes for everything to start running.

The Hasura console will be at:

.. code-block:: bash

   http://your_droplet_ip/console


The GraphQL Endpoint will be:

.. code-block:: bash

   http://your_droplet_ip/v1alpha1/graphql


A Postgres database is also provisioned on the Droplet. Using the console, you
can create a table on this Postgres instance and make your first GraphQL query.

.. image:: https://graphql-engine-cdn.hasura.io/heroku-repo/assets/hasura_console.png
   :alt: Hasura console

3. Create a table
~~~~~~~~~~~~~~~~~

Navigate to ``Data -> Create table`` on the console and create a table called ``profile`` with the following columns:

``profile``

===============  ========
 column name      type
===============  ========
``id``             Integer (auto-increment)
``name``           Text
===============  ========

Choose ``id`` as the Primary key and click the ``Create`` button.

.. image:: https://graphql-engine-cdn.hasura.io/heroku-repo/assets/hasura_create_table.png
   :alt: Hasura console - create table

4. Insert sample data
~~~~~~~~~~~~~~~~~~~~~

Once the table is created, go to the ``Insert Row`` tab and insert some sample rows:

.. code-block:: bash

   Thor
   Iron Man
   Hulk
   Captain America
   Black Widow

.. image:: https://graphql-engine-cdn.hasura.io/heroku-repo/assets/hasura_insert_row.png
   :alt: Hasura console - insert data

5. Try out GraphQL
~~~~~~~~~~~~~~~~~~

Switch to the ``GraphiQL`` tab on top and execute the following GraphQL query:

.. code-block:: graphql

   query {
     profile {
       id
       name
     }
   }

.. image:: https://graphql-engine-cdn.hasura.io/heroku-repo/assets/hasura_graphql_query.png
   :alt: Hasura console - GraphiQL

Secure the GraphQL endpoint
---------------------------

By default Hasura is exposed without any access key. Anyone can read and write
to your database using GraphQL. When deploying to production, you should secure
the endpoint by adding an access key and then setting up permission rules on
tables.

To add an access key, follow the steps given below:

1. Connect to the Droplet via SSH:

   .. code-block:: bash

      ssh root@your_droplet_ip


2. Goto ``/etc/hasura`` directory:

   .. code-block:: bash

      cd /etc/hasura


3. Edit ``docker-compose.yaml`` and un-comment the line that mentions access key.
   Also change it to some unique secret:

   .. code-block:: bash

      vim docker-compose.yaml

      ...
      # un-comment next line to add an access key
      HASURA_GRAPHQL_ACCESS_KEY: mysecretaccesskey
      ...

      # type ESC followed by :wq to save and quit


4. Update the container:

   .. code-block:: bash

      docker-compose up -d


That's it. Visit the console at ``http://your_droplet_ip/console`` and it should
prompt for the access key. Further API requests can be made by adding the
following header:

.. code-block:: bash

   X-Hasura-Access-Key: mysecretaccesskey


Adding a domain & Enabling HTTPS
--------------------------------

If you own a domain, you can enable HTTPS on this Droplet by mapping the domain
to the Droplet's IP. The One-Click Droplet is configured with Caddy which is an
HTTP/2 web server with automatic HTTPS using Let's Encrypt.

1. Go to your domain's DNS dashboard and add an A record mapping the domain to the Droplet IP.
2. Connect to the Droplet via SSH:

   .. code-block:: bash

      ssh root@your_droplet_ip


3. Goto ``/etc/hasura`` directory:

   .. code-block:: bash

      cd /etc/hasura


4. Edit ``Caddyfile`` and change ``:80`` to your domain:

   .. code-block:: bash

      vim Caddyfile

      ...
      add_your-domain-here {
        proxy / graphql-engine:8080 {
          websocket
        }
      }
      ...

      # type ESC followed by :wq to save and quit


5. Restart the container:

   .. code-block:: bash

      docker-compose restart caddy


Visit ``https://your_domain/console`` to visit the Hasura console.

Updating to the latest version
------------------------------

When a new version of GraphQL Engine is released, you can upgrade to it by just
changing the version tag in docker-compose.yaml. You can find the latest
releases on the GitHub releases page.

1. Connect to the Droplet via SSH:

   .. code-block:: bash

      ssh root@your_droplet_ip


2. Goto ``/etc/hasura`` directory:

   .. code-block:: bash

      cd /etc/hasura


3. Edit ``docker-compose.yaml`` and change the image tag to the latest one:

   .. code-block:: bash

      vim docker-compose.yaml

      ...
      graphql-engine:
        image: hasura/graphql-engine:latest_tag_here
      ...

      # type ESC followed by :wq to save and quit


4. Restart the container:

   .. code-block:: bash

      docker-compose up -d


Using a different database
--------------------------

1. Connect to the Droplet via SSH:

   .. code-block:: bash

      ssh root@your_droplet_ip


2. Goto ``/etc/hasura`` directory:

   .. code-block:: bash

      cd /etc/hasura

3. Setup the database that you wish to use, preferably via Docker Compose

4. Edit ``docker-compose.yaml`` and change the database URL:

   .. code-block:: bash

      vim docker-compose.yaml

      ...
      # change the url to use a different database
      HASURA_GRAPHQL_DATABASE_URL: postgres://<new-database-url>/<new-database-name>
      ...

      # type ESC followed by :wq to save and quit


Logs
----


1. Connect to the Droplet via SSH:

   .. code-block:: bash

      ssh root@your_droplet_ip


2. Goto ``/etc/hasura`` directory:

   .. code-block:: bash

      cd /etc/hasura

3. To checks logs for any container, use the following command:

   .. code-block:: bash

      docker-compose logs <container_name>

Where ``<container_name>`` is one of ``graphql-engine``, ``postgres`` or
``caddy``.

License
-------
The Hasura GraphQL Engine is open source. View license `here <https://github.com/hasura/graphql-engine/blob/master/LICENSE>`_.

