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

Introduction
------------

This guide walks you through deploying the Hasura GraphQL engine on `Azure    <https://azure.microsoft.com>`__ using `Container Instances
<https://azure.microsoft.com/en-us/services/container-instances/>`__ with `Azure Database for PostgreSQL server <https://azure.microsoft.com/en-us/services/postgresql/>`__.

One-click deployment using ARM Template
---------------------------------------

You can deploy all the resources mentioned in this guide with the one-click button below.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: With a new Postgres Server

     .. image:: /img/graphql/core/deployment/deploybutton.png
       :width: 200px
       :alt: azure_deploy_button_new_pg
       :class: no-shadow
       :target: https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fstable%2finstall-manifests%2fazure-container-with-pg%2fazuredeploy.json
     
     (This button takes you to the Azure Portal. You might want to :kbd:`Ctrl+Click` to
     open it in a new tab. Read more about this Resource Manager Template `here <https://github.com/hasura/graphql-engine/tree/stable/install-manifests/azure-container-with-pg>`__).

  .. tab:: With an existing Postgres Server

     .. image:: /img/graphql/core/deployment/deploybutton.png
       :width: 200px
       :alt: azure_deploy_button_existing_pg
       :class: no-shadow
       :target: https://portal.azure.com/#create/Microsoft.Template/uri/https%3a%2f%2fraw.githubusercontent.com%2fhasura%2fgraphql-engine%2fstable%2finstall-manifests%2fazure-container%2fazuredeploy.json
     
     (This button takes you to the Azure Portal. You might want to :kbd:`Ctrl+Click` to
     open it in a new tab. Read more about this Resource Manager Template `here <https://github.com/hasura/graphql-engine/tree/stable/install-manifests/azure-container>`__).


Pre-requisites
--------------

- Valid Azure subscription with billing enabled or credits (`click
  here <https://azure.microsoft.com/en-us/free/>`__ for a free trial).
- `Azure CLI <https://docs.microsoft.com/en-us/cli/azure/install-azure-cli>`__.

The actions mentioned below can be executed using the Azure Portal and the Azure CLI. But for the sake of simplicity in documentation, we are using Azure CLI; so that commands can be easily copy-pasted and executed.

Once you install the ``CLI``, login to your Azure account:

.. code-block:: bash

   az login

Create a new Resource Group
---------------------------

The Resource Groups are used to group various resources on Azure. Create a resource group called ``hasura`` at the ``westus`` location.

.. code-block:: bash

   az group create --name hasura --location westus

Provision a PostgreSQL server
-----------------------------

.. note::

   If you already have a database setup, you can skip these steps and jump directly to :ref:`azure_allow_access`.

After you create the resource group, create a Postgres server instance:

.. code-block:: bash

   az postgres server create --resource-group hasura \
      --name "<server_name>" \
      --location westus \
      --admin-user hasura \
      --admin-password "<server_admin_password>" \
      --sku-name GP_Gen5_2 \
      --version 10

.. note::

   - Choose a unique name for ``<server_name>`` variable.
   - Select a strong password for the ``<server_admin_password>`` variable, that must include  uppercase, lowercase, and numeric characters. You will need the password later to connect to the database. (make sure you escape the special characters depending on your shell).

Note the hostname in the output, as indicated below:

.. code-block:: bash

     ...
     "fullyQualifiedDomainName": "<server_name>.postgres.database.azure.com",
     ...

where; ``<server_name>.postgres.database.azure.com`` is the hostname.

.. note::

   If you get an error saying ``Specified server name is already used``, change the value of ``--name`` (``<server_name>``) to something else.

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

Create a firewall rule allowing access from Azure internal services:

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
      --environment-variables "HASURA_GRAPHQL_SERVER_PORT"="80" "HASURA_GRAPHQL_ENABLE_CONSOLE"="true" "HASURA_GRAPHQL_ADMIN_SECRET"="<admin-secret>"\
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

If the ``<dns-name-label>`` is unavailable, choose another unique name and re-execute the command.

Setting up JWT
--------------

Use the ``HASURA_GRAPHQL_JWT_SECRET`` env variable to setup JWT. To generate a new JWT config refer :ref:`here <generating_jwt_config>`.

.. note::
   
   The ``HASURA_GRAPHQL_JWT_SECRET`` env variable requires JSON format.
   To create a container group with  ``az container create --environment-variables`` flag you need to pass the variable as a key-value pair.

In order to use the env variable with the ``az container create`` command, pass the `JSON` value for the env variable as a string by escaping the characters like so:

.. code-block:: bash
   :emphasize-lines: 9

   az container create --resource-group hasura \
         --name hasura-graphql-engine \
         --image hasura/graphql-engine \
         --dns-name-label "<dns-name-label>" \
         --ports 80 \
         --environment-variables "HASURA_GRAPHQL_SERVER_PORT"="80" \ 
         "HASURA_GRAPHQL_ENABLE_CONSOLE"="true" \ 
         "HASURA_GRAPHQL_ADMIN_SECRET"="<admin-secret>" \
         "HASURA_GRAPHQL_JWT_SECRET"= \ "{\"type\": \"RS512\",\"key\": \"-----BEGIN CERTIFICATE-----\\nMIIDBzCCAe+gAwIBAgIJTpEEoUJ/bOElMA0GCSqGSIb3DQEBCwUAMCExHzAdBgNV\\nBAMTFnRyYWNrLWZyOC51cy5hdXRoMC5jb20wHhcNMjAwNzE3MDYxMjE4WhcNMzQw\\nMzI2MDYxMjE4WjAhMR8wHQYDVQQDExZ0cmFjay1mcjgudXMuYXV0aDAuY29tMIIB\\nIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAuK9N9FWK1hEPtwQ8ltYjlcjF\\nX03jhGgUKkLCLxe8q4x84eGJPmeHpyK+iZZ8TWaPpyD3fk+s8BC3Dqa/Sd9QeOBh\\nZH/YnzoB3yKqF/FruFNAY+F3LUt2P2t72tcnuFg4Vr8N9u8f4ESz7OHazn+XJ7u+\\ncuqKulaxMI4mVT/fGinCiT4uGVr0VVaF8KeWsF/EJYeZTiWZyubMwJsaZ2uW2U52\\n+VDE0RE0kz0fzYiCCMfuNNPg5V94lY3ImcmSI1qSjUpJsodqACqk4srmnwMZhICO\\n14F/WUknqmIBgFdHacluC6pqgHdKLMuPnp37bf7ACnQ/L2Pw77ZwrKRymUrzlQID\\nAQABo0IwQDAPBgNVHRMBAf8EBTADAQH/MB0GA1UdDgQWBBSOG3E+4lHiI+l0i91u\\nxG2Rca2NATAOBgNVHQ8BAf8EBAMCAoQwDQYJKoZIhvcNAQELBQADggEBAKgmxr6c\\nYmSNJOTPtjMFFDZHHX/7iwr+vqzC3nalr6ku8E3Zs0/IpwAtzqXp0eVVdPCWUY3A\\nQCUTt63GrqshBHYAxTbT0rlXFkqL8UkJvdZQ3XoQuNsqcp22zlQWGHxsk3YP97rn\\nltPI56smyHqPj+SBqyN/Vs7Vga9G8fHCfltJOdeisbmVHaC9WquZ9S5eyT7JzPAC\\n5dI5ZUunm0cgKFVbLfPr7ykClTPy36WdHS1VWhiCyS+rKeN7KYUvoaQN2U3hXesL\\nr2M+8qaPOSQdcNmg1eMNgxZ9Dh7SXtLQB2DAOuHe/BesJj8eRyENJCSdZsUOgeZl\\nMinkSy2d927Vts8=\\n-----END CERTIFICATE-----\"}"
         --secure-environment-variables "HASURA_GRAPHQL_DATABASE_URL"="<database-url>" 

.. note::
   
   Check out the :ref:`Running with JWT <running_with_jwt>` section for the usage of ``HASURA_GRAPHQL_JWT_SECRET`` env variable.

Open the Hasura Console
-----------------------

That's it! Once the deployment is complete, navigate to the container instance's IP or hostname to open the Hasura console:

.. code-block:: bash

   az container show --resource-group hasura \
      --name hasura-graphql-engine \
      --query "{FQDN:ipAddress.fqdn,ProvisioningState:provisioningState}" \
      --out table

The output contains the FQDN in the format ``<dns-name-label>.westus.azurecontainer.io``.

Visit the following URL for the Hasura console:

.. code:: 

   http://<dns-name-label>.westus.azurecontainer.io/console

Replace ``<dns-name-label>`` with the label given earlier.

.. image:: https://storage.googleapis.com/graphql-engine-cdn.hasura.io/main-repo/img/azure_arm_aci_console_graphiql.png
   :class: no-shadow
   :alt: Hasura console
   
You can create tables and test your GraphQL queries here.   

Troubleshooting
---------------

If your password contains special characters, check if the password is URL encoded and given as env variables. Also, check for proper escaping of these characters based on your shell.

You can check the logs to see if the database credentials are proper and if Hasura is able to connect to the database.

If you're using an existing/external database, make sure the firewall rules for the database allows connection for Azure services.

.. _azure_logs:

Checking logs
^^^^^^^^^^^^^

If the console doesn't load, check the logs:

.. code-block:: bash

   az container logs --resource-group hasura \
      --name hasura-graphql-engine \
      --container-name hasura-graphql-engine
   # use --follow flag to stream logs

Tearing down
------------

To clean up the deployment, delete the resource group:

.. code-block:: bash

   az group delete --resource-group hasura

References
----------

- `Installing Azure CLI <https://docs.microsoft.com/en-us/cli/azure/install-azure-cli>`_
- `Creating a Azure Postgres Server
  <https://docs.microsoft.com/en-us/azure/postgresql/quickstart-create-server-database-azure-cli>`_
- `Using Azure Container Instances
  <https://docs.microsoft.com/en-us/azure/container-instances/container-instances-quickstart>`_
