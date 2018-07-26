Quickstart with Docker
======================

This guide will help you get Hasura GraphQL engine and Postgres running as Docker containers using Docker Compose.
This is the easiest way of setting up Hasura GraphQL engine on your **local environment**.

In case, you'd like to run Hasura on an existing Postgres database, follow this guide to
:doc:`deploy the Hasura GraphQL engine as a standalone docker container <../deployment/docker/index>`
and connect it to your Postgres instance.

**Prerequisites**:

- `Docker <https://docs.docker.com/install/>`_
- `Docker Compose <https://docs.docker.com/compose/install/>`_

Step 1: Install the Hasura CLI
------------------------------

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Mac

      In your terminal enter the following command:

      .. code-block:: bash

         curl -L https://cli.hasura.io/install.sh | bash

      This will install the Hasura CLI in ``/usr/local/bin``. You might have to provide
      your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

   .. tab:: Linux

      Open your linux shell and run the following command:

      .. code-block:: bash

         curl -L https://cli.hasura.io/install.sh | bash

      This will install the Hasura CLI tool in ``/usr/local/bin``. You might have to provide
      your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

   .. tab:: Windows

      .. note::

         You should have ``git bash`` installed to use Hasura CLI. Download ``git bash`` using this `link
         <https://git-scm.com/download/win>`_. Also, make sure you install it in ``MinTTY`` mode, instead of Windows'
         default console.

      Download the ``hasura`` installer:

      * `hasura (64-bit Windows installer) <https://cli.hasura.io/install/windows-amd64>`_
      * `hasura (32-bit Windows installer) <https://cli.hasura.io/install/windows-386>`_

      **Note:** Please run the installer as ``Administrator`` to avoid PATH update errors. If you're still
      getting a "command not found" error after installing Hasura CLI, please restart ``git bash``.


Step 2: Initialize a project
----------------------------

.. code-block:: none

  hasura init --directory my-project

Step 3: Run Hasura GraphQL engine & Postgres
--------------------------------------------

.. code-block::  none

   cd my-project/install-scripts
   docker-compose up -d

Check if the containers are running:

.. code-block:: none

  $ docker ps

  CONTAINER ID IMAGE                 ... CREATED STATUS PORTS          ...
  097f58433a2b hasura/graphql-engine ... 1m ago  Up 1m  8080->8080/tcp ...
  b0b1aac0508d postgres              ... 1m ago  Up 1m  5432/tcp       ...

Step 4: Open the Hasura console
-------------------------------

Head to http://localhost:8080/console to open the Hasura console.

Next: Make your first GraphQL query!
------------------------------------

Next, make your :doc:`first graphql query <first-graphql-query>`.

Advanced:
---------

This was a quickstart guide to get the Hasura GraphQL engine up and running quickly. For more detailed instructions
on deploying using Docker, check out :doc:`../deployment/docker/index`
