Deploying Hasura GraphQL engine on Render
=========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide talks about how to deploy the Hasura GraphQL engine on `Render
<https://render.com>`__.


Pre-requisites
--------------

- GitHub or GitLab account
- Render account associated with the above Github or Gitlab account and with billing enabled


Create a repository
-------------------

- Create a new GitHub or GitLab repository
- Within the new repository, create a new file named ``Dockerfile`` that
references the latest Hasura GraphQL engine Docker image by including the
following single line in the file:

.. code-block:: bash

   FROM hasura/graphql-engine:latest


Using render.yaml (Fastest)
---------------------------------------------

In the same repository, create a second new file named 
``render.yaml``. Insert the following configuration:

.. code-block:: yaml

   services:
   - type: web
     name: hasura
     env: docker
     healthCheckPath: /
     envVars:
     - key: HASURA_GRAPHQL_DATABASE_URL
       fromDatabase:
         name: hasura
         property: connectionString
     - key: HASURA_GRAPHQL_ENABLE_CONSOLE
       value: true
   databases:
   - name: hasura

On the Render dashboard, find the **YAML** section and the **New from 
YAML** button or `go directly there <https://dashboard.render.com/select-repo?type=iac>`_ 
and select the repository you've created.

That's it! Skip to notes about the Hasura console and debug with the logs below.


Provision the database and web service manually (alternative method)
--------------------------------------------------------------------

.. note::

   If you already have a Render database setup, you can skip the first step.

- Create a Postgres server by providing a name in Render's `New Database 
configuration page <https://dashboard.render.com/new/database>`__.

- Once created, you'll see an internal connection string which 
is the database URL used in the next step.

- Create a web service by selecting the repository you just created in the `New Web 
Service configuration page <https://dashboard.render.com/select-repo?type=web>`__. 
You have to specify a name.

- Upon creation of the web service, find the environment tab and create two 
environment variables: one named ``HASURA_GRAPHQL_DATABASE_URL`` whose value is 
the internal connection string from the database you created earlier, 
and another named ``HASURA_GRAPHQL_ENABLE_CONSOLE`` whose value is ``true``.


Open the Hasura Console
-----------------------

That's it! You can monitor the deployment of the web service from the logs tab. 
There you can see error messages if the deployment fails. If the deployment succeeds, you 
follow the link at the top of the web service detail page or visit the following 
URL for the Hasura console:

.. code:: 

   http://<web-service-name>.onrender.com

Replace ``<web-service-name>`` with the name specified earlier.

You can create tables and test your GraphQL queries here. Check out :ref:`Making
your first GraphQL Query <first_graphql_query>` for a detailed example.


Tearing down
------------

To clean up, delete the web service and the database from the Render dashboard.


References
----------

- `Render Docs <https://render.com/docs>`_
