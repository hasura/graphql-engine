.. .. meta::
   :description: Manual for accessing postgres directly
   :keywords: hasura, docs, postgres, tunnel

Accessing Postgres directly
===========================

The postgres database runs as a microservice on the cluster.

Getting direct CLI access:
--------------------------

To get direct CLI access to the database, run the following hasura CLI command from your project directory:

.. code-block:: bash

   # Open a tunnel to postgres
   hasura microservice port-forward postgres -n hasura --local-port 6432

The above command will allow direct access to postgres at ``localhost:6432``.

Now, you can run ``psql`` (or any other postgres client) from your localhost to access the ``hasuradb`` database:

.. code-block:: bash

   psql -U admin -h localhost -p 6432 -d hasuradb

Accessing database directly from a microservice:
------------------------------------------------

See: :doc:`../microservices/connect-postgres`

.. ..todo::
   * Describe postgres, data API, and API gateway architecture
