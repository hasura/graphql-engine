.. .. meta::
   :description: How microservices work on a Hasura cluster
   :keywords: hasura, getting started, step 2

Communicating between microservices
===================================

Every microservice has an ``internal endpoint`` at which other microservices
can reach it. The internal URL is typically of the structure: ``http://<microservice-name>.<namespace>``.
This microservice discovery is powered by kubernetes's internal DNS.

Examples:

1. Hasura Data API: ``http://data.hasura``
2. Custom webapp called my-app: ``http://my-app.default``

This is because Hasura microservices run in the ``hasura`` namespace and custom microservices run in the ``default`` namespace.

Also every microservice expects a ``X-Hasura-User-Id`` and ``X-Hasura-Role`` header to be passed with the request for authentication. The ``X-Hasura-Role`` can be set to ``anonymous`` or ``admin`` depending on the service's access permissions. The ``X-Hasura-User-Id`` can be set to ``0``.

In fact, when a microservice is ``exposed externally`` on a route, the API gateway proxies the request to the internal URL of the microservice while setting the ``X-Hasura-User-Id`` and ``X-Hasura-Role`` headers based on the user session token passed.

To get the URLs of externally exposed microservices running on a hasura cluster run:

.. code-block:: bash

   $ hasura microservice list
   • Getting microservices...
   • Custom microservices:
   NAME      STATUS    INTERNAL-URL      EXTERNAL-URL
   my-app    Running   my-app.default    http://my-app.gram29.hasura-app.io

   • Hasura microservices:
   NAME            STATUS    INTERNAL-URL           EXTERNAL-URL
   auth            Running   auth.hasura            http://auth.gram29.hasura-app.io
   data            Running   data.hasura            http://data.gram29.hasura-app.io
   filestore       Running   filestore.hasura       http://filestore.gram29.hasura-app.io
   gateway         Running   gateway.hasura
   le-agent        Running   le-agent.hasura
   notify          Running   notify.hasura          http://notify.gram29.hasura-app.io
   platform-sync   Running   platform-sync.hasura
   postgres        Running   postgres.hasura
   session-redis   Running   session-redis.hasura
   sshd            Running   sshd.hasura


Contacting other microservices during local development of a microservice
-------------------------------------------------------------------------

To contact microservices on a cluster from your localhost while development you need to port forward the service from the cluster to your localhost


Examples:

.. code-block:: bash

   # port forward the postgres microservice in namespace hasura to local port 6432
   $ hasura microservice port-forward postgres -n hasura --local-port 6432

   # port forward the custom microservice <my-app> to local port 8080
   $ hasura microservice port-forward <my-app> --local-port=8080


Run the command ``hasura microservice port-forward -h`` for more information on the command options.
