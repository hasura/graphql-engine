.. meta::
   :description: Fetching logs of a service in hasura
   :keywords: hasura, logs, service


.. _fetching_logs:

Logs
======

To check the logs of any service running on a cluster, run:

.. code-block:: bash

   $ hasuractl logs <pod-name> -c <cluster-alias> -n <namepace>

To list all running instances of your services and get the required <pod-name>, run:

.. code-block:: bash

   $ hasuractl cluster status --detail -c <cluster-alias>

The output might look something like this:

.. code::

    ...
    ...
    INFO Hasura services:                             
    POD NAME                         STATUS
    auth-1949978544-7w4qv            Running
    console-1512529858-53clt         Running
    ...

    INFO Custom services:                             
    POD NAME               STATUS
    blog-910933640-k5pvm   Pending
    ...

To fetch logs of a custom service, say blog, run:

.. code-block:: bash

   $ hasuractl logs blog-910933640-k5pvm -c <cluster-alias>

This would output all the logs of that pod.

If you are interested in the logs of any hasura service, say auth, run:

.. code-block:: bash

   $ hasuractl logs auth-1949978544-7w4qv -c <cluster-alias> -n hasura

