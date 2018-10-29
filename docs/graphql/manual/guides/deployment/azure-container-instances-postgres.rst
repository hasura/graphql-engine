.. _deploy_azure_ci_pg:

Hasura GraphQL Engine on Azure with Container Instances and Postgres
====================================================================

This guide talks about how to deploy Hasura GraphQL Engine on `Azure
<https://azure.microsoft.com>`_ using `Container Instances
<https://azure.microsoft.com/en-us/services/container-instances/>`_ with `Azure
Database for PostgreSQL server <https://azure.microsoft.com/en-us/services/postgresql/>`_.

Contents
--------

* Pre-requisites
* Creating a Resource Group
* Provisioning a Postgres database on Azure
* Launching GraphQL Engine using ACI
* Using an existing Postgres database on Azure

Pre-requisites
--------------

The actions mentioned here can be execute using Azure Portal and Azure CLI. But,
for the sake of simplicity in documentation, we are going to use Azure CLI, so
that commands can be easily copy pasted and executed.

- Valid Azure Subscription with billing enabled or credits
- `Azure CLI <https://docs.microsoft.com/en-us/cli/azure/install-azure-cli>`_

Once the CLI is installed, login to your Azure account:

.. code-block:: bash

   az login

Creating a Resource Group
-------------------------

As the name suggestes, Resource Groups are used to group together various
resources on Azure. We'll create a resource group called ``hasura`` at
``westus`` location.

.. code-block:: bash

   az group create --name hasura --location westus

Provisioning a PostgreSQL server
--------------------------------

Once the resource group is created, we create a Postgres server instance:

.. code-block:: bash

   az postgres server create --resource-group hasura \
      --name "<server_name>" \
      --location westus \
      --admin-user hasura \
      --admin-password "<server_admin_password>" \
      --sku-name GP_Gen4_2 \
      --version 10

.. note::

   Choose a unique name for ``<server_name>``. Also choose a strong password for
   ``<server_admin_password>``, including uppercase, lowercase, numeric and
   special characters. This is will be required later connect to the database.
   Make sure you escape the special characters depending on your shell. 

Note down the hostname. It will be shown as below in the output:

.. code-block:: bash

     "fullyQualifiedDomainName": "hasura-postgres.postgres.database.azure.com",

``hasura-postgres.postgres.database.azure.com`` is the hostname here.

.. note::

   If you get an error saying ``Specified server name is already used``, change
   the value of ``--name`` to something else.

Create a new database
---------------------

Create a new database on the server:

.. code-block:: bash

   az postgres db create --resource-group hasura \
      --server-name "<server_name>" \
      --name hasura-db

Allow access to Azure Services
------------------------------

Go to `Azure Portal <https://portal.azure.com>`_ and navigate to the resource
group we just created and then to the Postgres Server. 

Click on ``Connection security`` under ``Settings`` section on the side bar.

Set ``Allow access to Azure services`` to ``ON`` and then click the ``Save``
button above.

Create a Container Instance
---------------------------

Let's launch Hasura using container instances:

.. code-block:: bash

   az container create --resource-group hasura \
      --name hasura-graphql-engine \
      --image hasura/graphql-engine \
      --dns-name-label hasura \
      --ports 8080 \
      --secure-environment-variables "HASURA_GRAPHQL_DATABASE_URL=<database-url>"

``<database-url>`` should be replaced by 

If the ``dns-name-label`` ``hasura`` is not available, choose another unique
name and execute the command again.

References
----------

- `Installing Azure CLI <https://docs.microsoft.com/en-us/cli/azure/install-azure-cli>`_
- `Creating a Azure Postgres Server
  <https://docs.microsoft.com/en-us/azure/postgresql/quickstart-create-server-database-azure-cli>`_
- `Using Azure Container Instances
  <https://docs.microsoft.com/en-us/azure/container-instances/container-instances-quickstart>`_
