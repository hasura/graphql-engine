.. .. meta::
:description: Listing created hasura clusters
:keywords: cluster, list

List running microservices
==========================

To get the list of all running microservices on a cluster, aliased say ``hasura``, use the hasura CLI.

.. code-block:: bash

  $ hasura microservices list -c hasura
  # or hasura ms list -c hasura

  USER MS NAME  STATUS   REPLICAS  INTERNAL-URL                   EXTERNAL-URL
  app           Running  1/1       app.alarming52-user:80  http://app.alarming52.hasura-app.io/

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
