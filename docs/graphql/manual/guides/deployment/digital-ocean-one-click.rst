Hasura GraphQL Engine One-click App on DigitalOcean Marketplace
===============================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Hasura GraphQL Engine is available as a One-click app on the DigitalOcean
Marketplace. It is packed with a `Postgres <https://www.postgresql.org/>`__
database and `Caddy <https://caddyserver.com/>`__ webserver for easy and
automatic HTTPS using `Let's Encrypt <https://letsencrypt.org/>`__.

Quickstart
----------

1. Create a Hasura One-click Droplet
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Click the button below to create a new Hasura GraphQL Engine Droplet through
DigitalOcean Marketplace. For first time users, the link also contains a
referral code with gives you $100 over days. A $5 droplet is good enough to
support most workloads. (``Ctrl+Click`` to open in a new tab)

.. image:: https://graphql-engine-cdn.hasura.io/img/create_hasura_droplet.png
   :width: 300px
   :alt: do_create_droplet_button
   :class: no-shadow
   :target: https://marketplace.digitalocean.com/apps/hasura?action=deploy&refcode=c4d9092d2c48&utm_source=hasura&utm_campaign=docs

2. Open console
~~~~~~~~~~~~~~~

Once Hasura GraphQL Engine Droplet is ready, you can visit the Droplet IP to
open the Hasura console, where you can create tables, explore GraphQL APIs etc.
Note that it might take 1 or 2 minutes for everything to start running.

The Hasura console will be at:

.. code-block:: bash

   http://<your_droplet_ip>/console


The GraphQL Endpoint will be:

.. code-block:: bash

   http://<your_droplet_ip>/v1/graphql


A Postgres database is also provisioned on the Droplet. Using the console, you
can create a table on this Postgres instance and make your first GraphQL query.

.. image:: https://graphql-engine-cdn.hasura.io/heroku-repo/assets/hasura_console.png
   :class: no-shadow
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
   :class: no-shadow
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
   :class: no-shadow
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
   :class: no-shadow
   :alt: Hasura console - GraphiQL

Secure the GraphQL endpoint
---------------------------

By default Hasura is exposed without any admin secret. Anyone can read and write
to your database using GraphQL. When deploying to production, you should secure
the endpoint by adding an admin secret key and then setting up permission rules on
tables.

To add an admin secret key, follow the steps given below:

1. Connect to the Droplet via SSH:

   .. code-block:: bash

      ssh root@<your_droplet_ip>


2. Goto ``/etc/hasura`` directory:

   .. code-block:: bash

      cd /etc/hasura


3. Edit ``docker-compose.yaml`` and un-comment the line that mentions admin secret key.
   Also change it to some unique secret:

   .. code-block:: bash

      vim docker-compose.yaml

      ...
      # un-comment next line to add an admin secret key
      HASURA_GRAPHQL_ADMIN_SECRET: myadminsecretkey
      ...

      # type ESC followed by :wq to save and quit


4. Update the container:

   .. code-block:: bash

      docker-compose up -d


That's it. Visit the console at ``http://<your_droplet_ip>/console`` and it should
prompt for the admin secret key. Further API requests can be made by adding the
following header:

.. code-block:: bash

   X-Hasura-Admin-Secret: myadminsecretkey


Adding a domain & Enabling HTTPS
--------------------------------

If you own a domain, you can enable HTTPS on this Droplet by mapping the domain
to the Droplet's IP. The Hasura GraphQL Droplet is configured with Caddy which is an
HTTP/2 web server with automatic HTTPS using Let's Encrypt.

1. Go to your domain's DNS dashboard and add an A record mapping the domain to the Droplet IP.
2. Connect to the Droplet via SSH:

   .. code-block:: bash

      ssh root@<your_droplet_ip>


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


Visit ``https://<your_domain>/console`` to visit the Hasura console.

Updating to the latest version
------------------------------

When a new version of GraphQL Engine is released, you can upgrade to it by just
changing the version tag in docker-compose.yaml. You can find the latest
releases on the `GitHub releases page
<https://github.com/hasura/graphql-engine/releases>`__.

1. Connect to the Droplet via SSH:

   .. code-block:: bash

      ssh root@<your_droplet_ip>


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


Using DigitalOcean Managed Postgres Database
--------------------------------------------

1. Create a new Postgres Database from DigitalOcean Console, preferably in the
   same region as the Droplet.
2. Once the database is created, under the "Overview" tab, from the "Connection
   Details" section, choose "Connection string" from the dropdown.
3. "Connection string" is the "Database URL" - copy it.
4. Connect to the Droplet via SSH:

   .. code-block:: bash

      ssh root@<your_droplet_ip>


5. Goto ``/etc/hasura`` directory:

   .. code-block:: bash

      cd /etc/hasura

6. Edit ``docker-compose.yaml`` and change the database URL:

   .. code-block:: bash

      vim docker-compose.yaml

      ...
      # change the url to use a different database
      HASURA_GRAPHQL_DATABASE_URL: <database-url>
      ...

      # type ESC followed by :wq to save and quit

Similarly, database URL can be changed to connect to any other Postgres
database.

.. note::

  If you're using Hasura with a restricted database user, make sure you go
  through :doc:`Postgres permissions <../../deployment/postgres-permissions>`
  to configure all required permissions. (Not applicable with the default
  connection string with DO Managed Postgres)

Logs
----


1. Connect to the Droplet via SSH:

   .. code-block:: bash

      ssh root@<your_droplet_ip>


2. Goto ``/etc/hasura`` directory:

   .. code-block:: bash

      cd /etc/hasura

3. To checks logs for any container, use the following command:

   .. code-block:: bash

      docker-compose logs <container_name>

Where ``<container_name>`` is one of ``graphql-engine``, ``postgres`` or
``caddy``.

Troubleshooting
---------------

Logs should be able to help you in most scenarios. If it doesn't, feel free to
talk to us on `Discord <https://discord.gg/hasura>`__.
