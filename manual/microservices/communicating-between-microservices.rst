.. .. meta::
   :description: How microservices work on a Hasura cluster
   :keywords: hasura, getting started, step 2

Communicating between microservices
===================================

Every microservice has an ``internal URL`` at which other microservices
can reach it. The URL is typically of the structure: ``http://<microservice-name>.<namespace>``.
This microservice discovery is powered by kubernetes's internal DNS.

Examples:

1. Hasura Data API: ``http://data.hasura``
2. Custom webapp called app: ``http://app.default``

This is because Hasura microservices run in the ``hasura`` namespace and custom microservices run in the ``default`` namespace.

Also every microservice expects a ``X-Hasura-User-Id`` and ``X-Hasura-Role`` header to be passed with the request for authentication. The ``X-Hasura-Role`` can be set to ``anonymous`` or ``admin`` depending on the service's access permissions. The ``X-Hasura-User-Id`` can be set to ``0``.

In fact, when a microservice is ``exposed externally`` on a route, the API gateway proxies the request to the internal URL of the microservice while setting the ``X-Hasura-User-Id`` and ``X-Hasura-Role`` headers based on the user session token passed.

To get the URLs of externally exposed microservices running on a hasura cluster run:

.. code-block:: bash

   $ hasura microservices list
   INFO Custom microservices:
   NAME   STATUS    URL
   app    Running   https://app.doyenne73.hasura-app.io

   INFO Hasura microservices:
   NAME            STATUS    URL
   auth            Running   https://auth.doyenne73.hasura-app.io
   data            Running   https://data.doyenne73.hasura-app.io
   filestore       Running   https://filestore.doyenne73.hasura-app.io
   gateway         Running
   le-agent        Running
   notify          Running   https://notify.doyenne73.hasura-app.io
   platform-sync   Running
   postgres        Running
   session-redis   Running
   sshd            Running
   vahana          Running


Contacting other microservices during local development of a microservice
-------------------------------------------------------------------------

To contact microservices on a cluster from your localhost while development you need to port forward the service from the cluster to your localhost


Examples:

.. code-block:: bash

   # port forward the postgres microservice in namespace hasura to local port 6432
   $ hasura microservice port-forward postgres -n hasura --local-port 6432

   # port forward the custom microservice www to local port 8080
   $ hasura microservice port-forward www --local-port=8080


Run the command ``hasura microservice port-forward -h`` for more information on the command options.
