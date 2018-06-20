Install Hasura GraphQL Engine using docker compose
==================================================

Prerequisites:
**************

To proceed with the installation, please ensure that you have a working setup of the following softwares

- `Docker <https://docs.docker.com/install/>`_
- `Docker Compose <https://docs.docker.com/compose/install/>`_

Step 1: Initialize a project directory
**************************************

.. code-block:: bash

  hasura init --directory my-project

Step 2: Install GraphQL Engine
*********************************

.. code-block:: bash

  $ cd my-project
  $ cd __install/docker-compose
  $ docker-compose up -d

Step 3: Validate the setup
**************************

Run the following command

.. code-block:: bash

  $ docker ps

You should be able to see the output as in the screenshot below.

.. image:: ../../../img/InstallSuccessDocker1.jpg

Please visit `http://localhost:8080 <http://localhost:8080>`_ and you should see the page as in the screenshot below.

.. image:: ../../../img/InstallSuccess.jpg
  :alt: Heroku installation success

Step 4: Open the hasura console
*******************************

In the my-project/config.yaml file set the endpoint:

.. code-block:: bash

  endpoint: http://localhost:8080

Now, open the hasura console:

.. code-block:: bash

  # Run this command in the my-project/ directory
  $ hasura console

Checkout our :doc:`../schema/index` section to know more about how to create tables, interact with them.
