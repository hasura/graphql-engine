.. .. meta::
   :description: Manual for accessing postgres directly
   :keywords: hasura, docs, postgres, tunnel

Accessing Postgres directly
===========================

Run the following ``hasura`` CLI commands, from your project directory:

.. code-block:: bash

   # Open a tunnel to postgres
   hasura microservice port-forward postgres -n hasura --local-port 6432

The command above will allow direct access to postgres at ``localhost:6432``.

Now, you can run psql (or any other postgres client) from your localhost to access the hasuradb database:

.. code-block:: bash

   psql -U admin -h localhost -p 6432 -d hasuradb



.. ..todo::
   * Describe postgres, data API, and API gateway architecture
   * Access postgres locally via `hasura forward`
   * Access postgres from an internal microservice via `postgres.hasura` and secrets
