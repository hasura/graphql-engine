Connecting to existing postgres
===============================

Lets say I have a postgres database running and I would like to connect Hasura GraphQL Engine to communicate with that database.

.. note:: 

  The `Step 0: Setting up postgres db`_ is used to set up a postgres database server, you can skip if you already have one running. 

Step 0: Setting up postgres db
******************************

Running postgres db
^^^^^^^^^^^^^^^^^^^

I am setting up my postgres container using the command below

.. code-block:: bash

  $ docker run --name hasura-postgres -e POSTGRES_PASSWORD=mysecretpassword -p 5432:5432 -d postgres

I am importing `this <https://raw.githubusercontent.com/xivSolutions/ChinookDb_Pg_Modified/pg_names/chinook_pg_serial_pk_proper_naming.sql>`_ schema.

.. code-block:: bash

  $ psql --echo-errors -h 127.0.0.1 -p 5432 -U postgres -d postgres < chinook_pg_serial_pk_proper_naming.sql
  ..
  ..
  ..
  CREATE INDEX
  CREATE TRIGGER
  CREATE TRIGGER
  ALTER TABLE
  CREATE INDEX
  CREATE TRIGGER
  ALTER TABLE

Step 1: Initialize a project directory
**************************************

.. code-block:: bash

  hasura-dev init --directory my-project

Step 2: Install GraphQL Engine
*********************************

.. code-block:: bash

  $ cd my-project
  $ cd __install/docker

Here is my ``docker-run.sh`` configuration which connects to my postgres database running locally

.. code-block:: bash

  docker run -p 8080:8080 \
         --link hasura-postgres:postgres \                                                                                                             
         hasuranightly/raven:8df5234 \
         raven \
         --database-url postgres://postgres:mysecretpassword@postgres:5432/postgres  \
         serve

Please modify the ``--database-url`` parameter in the above file to connect to your database.

Step 3: Open the hasura console
*******************************

In the my-project/config.yaml file set the endpoint:

.. code-block:: bash

  endpoint: http://localhost:8080

.. note::
  Please note that I have a running GraphQL engine on my local machine which is listening at port 8080

Now, open the hasura console:

.. code-block:: bash

  # Run this command in the my-project/ directory
  $ hasura-dev console

Navigate to `http://localhost:9695/data/schema <http://localhost:9695/data/schema>`_

You should see the list of tables which are yet to be tracked in your public schema. It should look like the screenshot below

.. image:: ../../../img/UntrackedTables.jpg
  :alt: List of untracked tables

Click on the ``Add all`` button as shown in the image below to track all the tables.

.. image:: ../../../img/TrackTable.jpg
  :alt: Track the list of untracked table

Once all the tables are tracked, the UI should look like below


.. image:: ../../../img/TableTracked.jpg
  :alt: Tables successfully tracked


Now we have successfully tracked the tables, lets open API Explorer and make a GraphQL query


.. image:: ../../../img/GraphQLAPI.jpg
  :alt: Make a simple fetch call to query actors

In the above screenshot, we are making a GraphQL query to fetch id, first_name, last_name from all the actors

.. code-block:: bash

  query {
    actors {
      id
      first_name
      last_name
    }
  }

and we received the following response

.. code-block:: bash

  {
    "data": {
      "actors": [
        {
          "id": 1,
          "first_name": "Penelope",
          "last_name": "Guiness"
        },
        // ...
      ]
    }
  }

