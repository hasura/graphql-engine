.. meta::
   :description: Fetching logs of a service in hasura
   :keywords: hasura, logs, service


.. _fetching_logs:

Logs
======

#. Install `kubectl <https://kubernetes.io/docs/tasks/tools/install-kubectl>`_.
#. Head to Console > Cluster Access > Kubectl. Follow the instructions to setup the access to cluster.

Now, to list all running instances of your services, run

.. code-block:: bash

   $ kubectl get po

The output might look something like this:

.. code::

   NAME                        READY     STATUS    RESTARTS   AGE
   buildbot-2115298977-wk6jk   1/1       Running   0          36d
   worker-865938653-cjsdj      1/1       Running   0          35d

To fetch logs of worker,

.. code-block:: bash

   $ kubectl logs worker-865938653-cjsdj

This would output all the logs of that pod. Instead, if we want to fetch the logs from the last 10m, we can run:

.. code-block:: bash

   $ kubectl logs worker-865938653-cjsdj --since=10m

If you are interested in the logs of any hasura service, first list out the current pods with

.. code-block:: bash

   $ kubectl get po -n hasura

and then fetch the logs of the interested pod by running:

.. code-block:: bash

   $ kubectl logs <pod-name> -n hasura
