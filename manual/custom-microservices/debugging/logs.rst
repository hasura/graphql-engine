.. .. meta::
   :description: How microservices work on a Hasura cluster
   :keywords: hasura, getting started, step 2

Viewing microservice logs
=========================

STDOUT output (``print()``, ``console.log()`` ) from microservices get
captured as logs.

The ``hasura microservice logs`` command will get logs for your microservice. Note: ``hasura ms`` is an alias for ``hasura microservice``, useful if you want to save on a few keystrokes.

**Fetch all the logs till now in one shot:**

.. code-block:: bash

   $ hasura ms logs app

**Fetch the last 100 log lines:**

.. code-block:: bash

   $ hasura ms logs app --tail 100

**Stream (follow) logs:**

The command will not exit and log lines will keep coming up on your terminal:

.. code-block:: bash

   $ hasura ms logs app -f

**Get logs of a hasura microservice, say auth**

.. code-block:: bash

   $ hasura ms logs auth -n hasura


For more details, read the full CLI reference for :doc:`hasura microservice logs <../../hasuractl/hasura_microservice_logs>`.
