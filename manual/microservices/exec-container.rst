.. .. meta::
   :description: How microservices work on a Hasura cluster
   :keywords: hasura, getting started, step 2

Exec / SSH into microservice containers
=======================================

If you've used typical hosting or cloud providers before, you might be used to having the ability to ``SSH`` into your server to be able to check up on environment variables, network reachability, ``top`` commands etc. You can do the same thing with Hasura, but the analog to ``SSH`` is to ``exec`` into a microservice's container.

``exec`` into a container basically means that you can ``exec`` whatever process or command you want, in the container. To make this equivalent to ``SSH``, ``exec`` a shell in your container!

**Open a bash shell in your microservice `app`:**

.. code-block:: bash

   $ hasura ms exec app -ti -- /bin/bash
   $> # This is a bash prompt in your container

**Open a shell in your microservice `app`:**

Sometimes, your container might not have ``bash``, and might only have ``sh``.

.. code-block:: bash

   $ hasura ms exec app -ti -- /bin/sh
   $ # This is a sh prompt in your container

**Open psql in your hasura `postgres` container:**

Sometimes, your container might not have ``bash``, and might only have ``sh``.

.. code-block:: bash

   $ hasura ms exec postgres -n hasura -ti -- /bin/bash
   root@postgres-3391217220-6jbq7:/$ #You can now run psql, pg_dump and other commands

For more details, read the full CLI reference for :doc:`hasura microservice exec <../hasuractl/hasura_microservice_exec>`.
