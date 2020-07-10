.. meta::
   :description: Deploy Hasura GraphQL engine on Azure with Container Instances and Postgres
   :keywords: hasura, docs, guide, deployment, azure, container, postgres

.. _deploy_azure_ci_pg:

Hasura GraphQL engine on Azure with Container Instances and Postgres
====================================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide talks about how to deploy the Hasura GraphQL engine on `Azure
<https://azure.microsoft.com>`__ using `Container Instances
<https://azure.microsoft.com/en-us/services/container-instances/>`__ with `Azure
Database for PostgreSQL server <https://azure.microsoft.com/en-us/services/postgresql/>`__.

One-click deploy using ARM Template
-----------------------------------

All resources mentioned in this guide can be deployed using the one-click button below.


.. rst-class:: api_tabs
.. tabs::

  .. tab:: With a new Postgres Server

     .. image:: https://azuredeploy.net/deploybutton.png
       :width: 200px
       :alt: azure_deploy_button_new_pg
       :class: no-shadow
       :target: https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fstable%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json
     
     (This button takes you to the Azure Portal, you might want to :kbd:`Ctrl+Click` to
     open it in a new tab. Read more about this Resource Manager Template `here <https://github.com/hasura/graphql-engine/tree/stable/install-manifests/azure-container-with-pg>`__).

  .. tab:: With an existing Postgres Server

     .. image:: https://azuredeploy.net/deploybutton.png
       :width: 200px
       :alt: azure_deploy_button_existing_pg
       :class: no-shadow
       :target: https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fstable%2finstall-manifests%2fazure-container%2fazuredeploy.json
     
     (This button takes you to the Azure Portal, you might want to :kbd:`Ctrl+Click` to
     open it in a new tab. Read more about this Resource Manager Template `here <https://github.com/hasura/graphql-engine/tree/stable/install-manifests/azure-container>`__).


Pre-requisites
--------------

- Valid Azure Subscription with billing enabled or credits (`click
  here <https://azure.microsoft.com/en-us/free/>`__ for a free trial).
- `Azure CLI <https://docs.microsoft.com/en-us/cli/azure/install-azure-cli>`__.

The actions mentioned below can be executed using the Azure Portal and the Azure CLI. But,
for the sake of simplicity in documentation, we are going to use Azure CLI, so
that commands can be easily copy-pasted and executed.

Once the CLI is installed, login to your Azure account:

.. code-block:: bash

   az login

Create a new Resource Group
---------------------------

As the name suggestes, Resource Groups are used to group together various
resources on Azure. We'll create a resource group called ``hasura`` at the
``westus`` location.

.. code-block:: bash

   az group create --name hasura --location westus

Provision a PostgreSQL server
-----------------------------

.. note::

   If you already have a database setup, you can skip these steps and jump
   directly to :ref:`azure_allow_access`.

Once the resource group is created, we create a Postgres server instance:

.. code-block:: bash

   az postgres server create --resource-group hasura \
      --name "<server_name>" \
      --location westus \
      --admin-user hasura \
      --admin-password "<server_admin_password>" \
      --sku-name GP_Gen5_2 \
      --version 10

.. note::

   Choose a unique name for ``<server_name>``. Also choose a strong password for
   ``<server_admin_password>``, including uppercase, lowercase and numeric characters.
   This will be required later to connect to the database
   (make sure you escape the special characters depending on your shell).

Note down the hostname. It will be shown as below in the output:

.. code-block:: bash

     ...
     "fullyQualifiedDomainName": "<server_name>.postgres.database.azure.com",
     ...

``<server_name>.postgres.database.azure.com`` is the hostname here.

.. note::

   If you get an error saying ``Specified server name is already used``, change
   the value of ``--name`` (``<server_name>``) to something else.

Create a new database
---------------------

Create a new database on the server:

.. code-block:: bash

   az postgres db create --resource-group hasura \
      --server-name "<server_name>" \
      --name hasura

.. _azure_allow_access:

Allow access to Azure Services
------------------------------

Create a firewall rule allowing acess from Azure internal services:

.. code-block:: bash

   az postgres server firewall-rule create --resource-group hasura \
      --server-name "<server_name>" \
      --name "allow-azure-internal" \
      --start-ip-address 0.0.0.0 \
      --end-ip-address 0.0.0.0

Create a Container Instance
---------------------------

Launch Hasura using a container instance:

.. code-block:: bash

   az container create --resource-group hasura \
      --name hasura-graphql-engine \
      --image hasura/graphql-engine \
      --dns-name-label "<dns-name-label>" \
      --ports 80 \
      --environment-variables "HASURA_GRAPHQL_SERVER_PORT"="80" "HASURA_GRAPHQL_ENABLE_CONSOLE"="true" \
      --secure-environment-variables "HASURA_GRAPHQL_DATABASE_URL"="<database-url>"

``<database-url>`` should be replaced by the following format:

.. code-block:: bash

   postgres://hasura%40<server_name>:<server_admin_password>@<hostname>:5432/hasura

If you'd like to connect to an existing database, use that server's database url.

.. note::

   ``%40`` is used in the username because Azure creates usernames as
   ``admin-user@server-name`` and since the database url uses ``@`` to separate
   username-password from hostname, we need to url-escape it in the username.
   Any other special character should be url-encoded.

If the ``<dns-name-label>`` is not available, choose another unique name and
execute the command again.

Open the Hasura Console
-----------------------

That's it! Once the deployment is complete, navigate to the container instance's
IP or hostname to open the Hasura console:

.. code-block:: bash

   az container show --resource-group hasura \
      --name hasura-graphql-engine \
      --query "{FQDN:ipAddress.fqdn,ProvisioningState:provisioningState}" \
      --out table

The output will contain the FQDN in the format
``<dns-name-label>.westus.azurecontainer.io``.

Visit the following URL for the Hasura console:

.. code:: 

   http://<dns-name-label>.westus.azurecontainer.io/console

Replace ``<dns-name-label>`` with the label given earlier.

.. image:: https://storage.googleapis.com/graphql-engine-cdn.hasura.io/main-repo/img/azure_arm_aci_console_graphiql.png
   :class: no-shadow
   :alt: Hasura console

You can create tables and test your GraphQL queries here. Check out :ref:`Making
your first GraphQL Query <first_graphql_query>` for a detailed guide.

Troubleshooting
---------------

If your password contains special characters, check if they were URL encoded
and given as environment variables. Also check for proper escaping of
these characters based on your shell.

You can check the logs to see if the database credentials are proper and if
Hasura is able to connect to the database.

If you're using an existing/external database, make sure the firewall rules for
the database allow connection for Azure services.

Checking logs
^^^^^^^^^^^^^

If the console is not loading, you might want to check the logs and see if something
is wrong:

.. code-block:: bash

   az container logs --resource-group hasura \
      --name hasura-graphql-engine \
      --container-name hasura-graphql-engine
   # use --follow flag to stream logs

Tearing down
------------

To clean-up, just delete the resource group:

.. code-block:: bash

   az group delete --resource-group hasura

References
----------

- `Installing Azure CLI <https://docs.microsoft.com/en-us/cli/azure/install-azure-cli>`_
- `Creating a Azure Postgres Server
  <https://docs.microsoft.com/en-us/azure/postgresql/quickstart-create-server-database-azure-cli>`_
- `Using Azure Container Instances
  <https://docs.microsoft.com/en-us/azure/container-instances/container-instances-quickstart>`_
