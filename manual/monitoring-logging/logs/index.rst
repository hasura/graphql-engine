.. .. meta::
   :description: Fetching logs of a microservice in hasura
   :keywords: hasura, logs, microservice


.. _fetching_logs:

Logs
======

To check the logs of any microservice running on a cluster, run:

.. code-block:: bash

   $ hasura microservice logs <microservice-name> -n <namespace>

To fetch logs of a custom microservice, say blog, run:

.. code-block:: bash

   $ hasura microservice logs blog

This would output all the logs of that pod.

If you are interested in the logs of any hasura microservice, say auth, run:

.. code-block:: bash

   $ hasura microservice logs auth -n hasura

