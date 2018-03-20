.. .. meta::
  :description: Listing created hasura clusters
  :keywords: cluster, list

Hasura microservices
====================

Hasura microservices are the default Hasura specific microservices that run on your cluster. These include the backend APIs provided by Hasura and other components of the Hasura platform.

Hasura microservices run in the **hasura namespace** on a cluster. Most ``hasura microservice *`` commands
will take the ``-n hasura`` flag while dealing with a service in the hasura namespace.

The following command lists the Hasura microservices that are running on a cluster along with their contact endpoints:

.. code-block:: bash

  $ hasura microservices list -n hasura    # optionally -c <cluster-alias>
  # or hasura ms list -n hasura

  HASURA MS NAME  STATUS   REPLICAS  INTERNAL-URL                                 EXTERNAL-URL
  auth            Running  1/1       auth.alarming52-hasura:80             http://auth.alarming52.hasura-app.io/
  data            Running  1/1       data.alarming52-hasura:80             http://data.alarming52.hasura-app.io/
  filestore       Running  1/1       filestore.alarming52-hasura:80        http://filestore.alarming52.hasura-app.io/
  gateway         Running  1/1
  le-agent        Running  1/1
  notify          Running  1/1       notify.alarming52-hasura:80           http://notify.alarming52.hasura-app.io/
  platform-sync   Running  1/1
  postgres        Running  1/1       postgres.alarming52-hasura:5432
  session-redis   Running  1/1       session-redis.alarming52-hasura:6379
  sshd            Running  1/1

The Hasura microservices that run on a cluster are:

auth
^^^^
The auth microservice handles user authentication for your Hasura cluster.

data
^^^^
The data microservice on Hasura exposes an HTTP/JSON API over a PostgreSQL database.

filestore
^^^^^^^^^
The filestore microservice lets users upload and store files on the Hasura project and also download when required.

gateway
^^^^^^^
This microservice exposes your project to the outside world. It authenticates and redirects external requests to the appropriate microservice inside the cluster.

le-agent
^^^^^^^^
This is the LetsEncrypt agent which is responsible for generating SSL certificates.

notify
^^^^^^
This microservice handles the Email/SMS communication.

platform-sync
^^^^^^^^^^^^^
This microservice keeps the cluster in sync with the configuration.

postgres
^^^^^^^^
This microservice hosts the PostgreSQL database.

session-redis
^^^^^^^^^^^^^
This is a Redis instance used by the auth microservice.

sshd
^^^^
This microservice handles SSH access to the cluster.