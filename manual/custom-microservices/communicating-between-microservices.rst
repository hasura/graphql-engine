.. meta::
   :description: How microservices work on a Hasura cluster
   :keywords: hasura, getting started, step 2

==================================
Communcating between microservices
==================================

Every microservice has an internal hostname and URL at which other microservices
can reach it. The URL is typically of the structure: `http://<microservice-name>.<namespace>`.
This service discovery is powered by kubernetes's internal DNS.

Examples:

1. Hasura Data API: `http://data.hasura`
2. Custom webapp called app: `http://app.default`

This is because Hasura microservices run in the ``hasura`` namespace and custom microservices run in the ``default`` namespace.

In fact, when a microservice is exposed 'externally' on a route, the API gateway proxies the request to the internal URL of the
microservice.

To get the internal and external URLs for the microservices running on the hasura cluster:

.. code-block::

   $ hasura microservices list
   INFO Custom microservices:
   NAME   STATUS    URL
   app    Running   https://app.h34-doyenne73-stg.hasura-app.io

   INFO Hasura microservices:
   NAME            STATUS    URL
   auth            Running   https://auth.h34-doyenne73-stg.hasura-app.io
   data            Running   https://data.h34-doyenne73-stg.hasura-app.io
   filestore       Running   https://filestore.h34-doyenne73-stg.hasura-app.io
   gateway         Running
   le-agent        Running
   notify          Running   https://notify.h34-doyenne73-stg.hasura-app.io
   platform-sync   Running
   postgres        Running
   session-redis   Running
   sshd            Running
   vahana          Running


Contacting other microservices during local development of a microservice
-------------------------------------------------------------------------

.. todo::

   Add to this section
