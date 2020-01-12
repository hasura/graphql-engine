Hasura GraphQL Engine on Render
====================================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide talks about how to deploy the Hasura GraphQL engine on `Render
<https://render.com>`__.


Pre-requisites
--------------

- GitHub (or alternative) account
- Render account associated with a Github account and with billing enabled


Create a repository
-------------------

Create a new GitHub repository

Within the new repository, create a new file named ``Dockerfile`` that 
references the latest Hasura GraphQL Engine docker image by including the 
following single line in the file:

.. code-block:: bash

   FROM hasura/graphql-engine:latest


Using Render Infrastrcuture as Code (Fastest)
---------------------------------------------

In the same repository, create a second new file, this one named 
``render.yaml``. Includes the following Render Infrastrcuture as 
Code configuration:

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

Within the Render Dashboard, find the YAML section and the New from 
YAML button or `go directly there <https://dashboard.render.com/select-repo?type=iac>`_ 
and select the repository you've created.

This it! Skip to notes about the Hasura Console and debugging with logs below.


Provision the Database and Web Service Manually (Alternate Method)
------------------------------------------------------------------

.. note::

   If you already have a Render database setup, you can skip this step.

Create a Postgres server by providing a name in Render's `New Database 
configuration page <https://dashboard.render.com/new/database>`__.

Once created, you'll see an Internal Connection String which 
is the database URL used in the next step.

Create a web service by selecting the repository you just created in the `New Web 
Service configuration page <https://dashboard.render.com/select-repo?type=web>`__. 
You have to specify a name.

Upon creation of the web service, find the Environment tab and create two 
environment variables: one named ``HASURA_GRAPHQL_DATABASE_URL`` whose value is 
the Internal Connection String from the database you created earlier; 
another named ``HASURA_GRAPHQL_ENABLE_CONSOLE`` whose value is ``true``.


Open the Hasura Console
-----------------------

That's it! You can monitor the deployment of the web service from the Logs tab. 
There you can see error messages if deployment fails. If deployment succeeds, you 
follow the link at the top of the web service detail page or visit the following 
URL for the Hasura console:

.. code:: 

   http://<web-service-name>.onrender.com

Replace ``<web-service-name>`` with the name specified earlier.

You can create tables and test your GraphQL queries here. Check out :ref:`Making
your first GraphQL Query <first_graphql_query>` for a detailed guide.


Tearing down
------------

To clean-up, delete the web service and the database from the Render Dashboard.


References
----------

- `Render Docs <https://render.com/docs>`_