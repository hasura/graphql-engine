.. meta::
   :description: Using Hasura with a YugaByte Postgres database
   :keywords: hasura, docs, existing database, guide, yugabyte

.. _cloud_db_yugabyte:

Using Hasura Cloud with a YugaByte Postgres database
====================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This guide explains how to connect a new or existing YugaByte Postgres database to a Hasura Cloud project.

Step 0: Sign up or log in to Hasura Cloud
-----------------------------------------

Navigate to `Hasura Cloud <https://cloud.hasura.io/>`__ and sign up or log in.

.. _create_hasura_project_yugabyte:

Step 1: Create a Hasura Cloud project
-------------------------------------

On the Hasura Cloud dashboard, create a new project:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/create-hasura-cloud-project.png
   :alt: Create Hasura Cloud project
   :width: 1000px

You will get prompted for a Postgres Database URL. We will create this in the next step and then come back here.

.. thumbnail:: /img/graphql/cloud/cloud-dbs/database-setup.png
   :alt: Hasura Cloud database setup
   :width: 500px

Also, copy the Hasura Cloud IP for later.

.. _create_pg_db_yugabyte:

Step 2: Create a Postgres DB on YugaByte (skip if you have an existing DB)
--------------------------------------------------------------------------

Log into the `YugaByte Cloud dashboard <https://cloud.yugabyte.com/login>`__.

On the YugaByte Cloud dashboard, click on ``Create cluster``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/yugabyte/create-cluster.png
   :alt: Create cluster on YugaByte
   :width: 1000px

Select a ``Cloud Provider`` and ``Region`` and then click ``Create Cluster``.

.. _construct_db_url_yugabyte:

Step 3: Construct the database connection URL
---------------------------------------------

The structure of the database connection URL looks as follows:

.. code-block:: bash

    postgresql://<user-name>:<password>@<public-ip>:<postgres-port>/<db>

To get it, click on your cluster on the cluster dashboard:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/yugabyte/go-to-cluster.png
   :alt: Go to cluster on YugaByte
   :width: 700px

On your cluster's dashboard, click on ``Connect`` on the top right:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/yugabyte/connect.png
   :alt: Connect to cluster on YugaByte
   :width: 1000px

Now you can get the connection info from the following screen:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/yugabyte/connection-info.png
   :alt: Connection info for YugaByte
   :width: 600px

- ``user-name``: If you have a separate database user, the user name will be their name. If you didn't specify a user, the default user name is ``admin`` (see after ``-U`` in the screenshot above).
- ``password``: If you have a separate database user, use their password. Otherwise, use the password is what follows ``PGPASSWORD=`` in the screenshot above.
- ``public-ip``: The public IP is what follows ``-h`` in the screenshot above.
- ``postgres-port``: The port is ``10301`` (see on the screenshot after ``-p``). This can be configured if required.
- ``db``: The DB is ``yugabyte`` (see on the screenshot after ``-d``).

Step 4: Finish creating the Hasura Cloud project
------------------------------------------------

Back on the Hasura Cloud dashboard, enter the database URL that we constructed in :ref:`step 3 <construct_db_url_yugabyte>`:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/finish-create-project.png
   :alt: Finish creating the Hasura Cloud project
   :width: 500px

Then click ``Create project``.

Step 5: Launch Hasura console
-----------------------------

After the project is initialized successfully, click on ``Launch console``:

.. thumbnail:: /img/graphql/cloud/cloud-dbs/launch-console.png
   :alt: Launch the Hasura console
   :width: 900px

Voilà. You are ready to start developing.

.. thumbnail:: /img/graphql/cloud/cloud-dbs/hasura-console.png
   :alt: Hasura console
   :width: 900px

Next steps
----------

You can check out our `30-Minute Hasura Basics Course <https://hasura.io/learn/graphql/hasura/introduction/>`__
and other `GraphQL & Hasura Courses <https://hasura.io/learn/>`__ for a more detailed introduction to Hasura.

You can also click the gear icon to manage your Hasura Cloud project. (e.g. add :ref:`collaborators <manage_project_collaborators>`,
:ref:`env vars <manage_project_env_vars>` or :ref:`custom domains <manage_project_domains>`) and :ref:`add an admin secret <secure_project>`
to make sure that your GraphQL endpoint and the Hasura console are not publicly accessible.

.. thumbnail:: /img/graphql/cloud/getting-started/project-manage.png
  :alt: Project actions
  :width: 860px

