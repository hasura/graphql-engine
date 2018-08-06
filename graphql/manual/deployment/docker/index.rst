Run Hasura GraphQL Engine using Docker
======================================

This guide assumes that you already have Postgres running and helps you set up the Hasura GraphQL engine using Docker
and connect it to your Postgres database.

**Prerequisites**:

- `Docker <https://docs.docker.com/install/>`_


Step 1: Get the bash script
---------------------------

.. code-block:: bash

   wget https://raw.githubusercontent.com/hasura/graphql-engine-install-manifests/master/docker-run/docker-run.sh


Step 2: Run the hasura docker container
---------------------------------------

Check the sample docker run command at ``docker-run.sh``.

Edit the ``--database-url`` flag command, so that you can connect to your Postgres instance.

.. code-block:: bash
   :emphasize-lines: 5

   #! /bin/bash
   docker run -d -p 8080:8080 \
          hasura/graphql-engine:latest \
          graphql-engine \
          --database-url postgres://username:password@hostname:port/dbname \
   serve --enable-console

Examples of ``database-url``:

- ``postgres://admin:password@localhost:5432/my-db``
- ``postgres://admin:@localhost:5432/my-db`` *(if there is no password)*

.. admonition:: Postgres on localhost

   If your Postgres database is running on ``localhost``, add the ``--net=host`` flag to allow the Docker container to
   access the host's network. This is what your command should look like:

   .. code-block:: bash
      :emphasize-lines: 2

      #! /bin/bash
      docker run -d --net=host \
             hasura/graphql-engine:latest \
             graphql-engine \
             --database-url postgres://username:password@hostname:port/dbname \
      serve --enable-console


Check if everything is running well:

.. code-block:: bash

   $ docker ps

   CONTAINER ID IMAGE                 ... CREATED STATUS PORTS          ...
   097f58433a2b hasura/graphql-engine ... 1m ago  Up 1m  8080->8080/tcp ...

Step 3: Open the hasura console
-------------------------------

In the ``my-project/config.yaml`` file set the endpoint:

.. code-block:: yaml

  endpoint: http://localhost:8080

Now, open the hasura console:

.. code-block:: bash

  # Run this command in the my-project/ directory
  $ hasura console

Step 4: Track existing tables and relationships
-----------------------------------------------

On the console page, you'll see your existing tables as "untracked tables" in the console.

.. image:: ../../../../img/graphql/manual/getting-started/TrackTable.jpg

Advanced:
---------

Checkout :ref:`Setting up migrations <migrations>` if you plan to track schema changes to your database.

.. toctree::
   :titlesonly:

   Securing your GraphQL endpoint <securing-graphql-endpoint>
   Updating GraphQL engine <updating>
