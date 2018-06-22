Run Hasura GraphQL Engine using Docker (Advanced)
=================================================

This guide assumes that you already have Postgres running and helps you set up Hasura GraphQL engine using Docker that talks to
your Postgres database.

In case you don't have Postgres running and want a quick setup, head to:
:doc:`Setup with Docker (Simple) <docker-simple>`.

**Prerequisites**:

- `Docker <https://docs.docker.com/install/>`_


Step 1: Install the Hasura CLI
------------------------------

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Mac

      In your terminal enter the following command:

      .. code-block:: bash

        curl -L https://storage.googleapis.com/hasuractl/install-dev.sh | bash

      As this is a preview release of the Hasura CLI, the CLI is named ``hasura-dev`` and not ``hasura``.

   .. tab:: Linux

      Open your linux shell and run the following command:

      .. code-block:: bash

        curl -L https://storage.googleapis.com/hasuractl/install-dev.sh | bash

      As this is a preview release of the Hasura CLI, the CLI is named ``hasura-dev`` and not ``hasura``.

   .. tab:: Windows

       Coming soon ...


Step 2: Initialise a Hasura project
-----------------------------------

Create a Hasura project directory.

.. code-block:: bash

  hasura-dev init --directory my-project

Step 3: Run the hasura docker container
---------------------------------------

You'll see a sample docker run command at ``my-project/__install/docker-run.sh``.

Edit the ``--database-url`` flag command, so that you can connect to your postgres instance.

.. code-block:: bash
   :emphasize-lines: 5

   #! /bin/bash
   docker run -p 8080:8080 \
       hasura/graphql-engine:latest \
       graphql-engine \
       --database-url postgres://username:password@hostname:port/dbname \
       serve

Examples of `database-url`:

- If the username and database is called admin: ``postgres://admin:password@localhost:5432/admin``
- If there is no password: ``postgres://admin:@localhost:5432/admin``

Check if everything is running well:

.. code-block:: bash

   $ docker ps

   CONTAINER ID IMAGE                 ... CREATED STATUS PORTS          ...
   097f58433a2b hasura/graphql-engine ... 1m ago  Up 1m  8080->8080/tcp ...

Step 4: Open the hasura console
-------------------------------

In the ``my-project/config.yaml`` file set the endpoint:

.. code-block:: bash

  endpoint: http://localhost:8080

Now, open the hasura console:

.. code-block:: bash

  # Run this command in the my-project/ directory
  $ hasura-dev console


Next, make your first GraphQL query
-----------------------------------

Next, make your :doc:`first graphql query<first-graphql-query>`.
