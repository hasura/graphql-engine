.. meta::
   :description: Fetching logs of a service in hasura
   :keywords: hasura, logs, service


.. _fetching_logs:

Logs
======

To check the logs of any service running on a cluster, run:

.. code-block:: bash

   $ hasura logs -s <service-name> -n <namepace>

To fetch logs of a custom service, say blog, run:

.. code-block:: bash

   $ hasuractl logs -s blog

This would output all the logs of that pod.

If you are interested in the logs of any hasura service, say auth, run:

.. code-block:: bash

   $ hasuractl logs -s auth -n hasura

