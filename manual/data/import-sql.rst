.. .. meta::
   :description: Manual for accessing postgres directly
   :keywords: hasura, docs, postgres, sql, import sql, data, import data

Importing data from SQL files
=============================

- Create a directory called ``sql`` in the Postgres microservice container.

.. code-block:: bash

   hasura microservice exec --namespace=hasura postgres -- mkdir /sql

- Copy your SQL file to this new directory in the container.

.. code-block:: bash

   hasura microservice cp <path-to-file>/data.sql hasura/postgres:/sql/data.sql

- Run this SQL file using ``psql``

.. code-block:: bash

   hasura ms exec --namespace=hasura postgres -- psql -U admin -d hasuradb -a -f "/sql/data.sql"