.. meta::
   :description: Docker networking with Hasura
   :keywords: hasura, docs, deployment, network, docker

.. _docker_networking:

Docker networking 
=================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Sometimes you might want to connect Hasura with APIs (e.g. auth webhooks, event triggers, remote schemas) that are either running outside of Docker or in a different Docker container.
Depending on the setting, the network config is different. This section shows how to connect in each of these use cases.

Network config
--------------

.. list-table:: 
   :stub-columns: 1
   :header-rows: 1

   * - 
     - Mac
     - Windows
     - Linux
     - Comment
   * - **Hasura to API (outside Docker)**
     - ``host.docker.internal:3000``
     - ``docker.for.win.localhost:3000``
     - ``localhost:3000`` with (``--net=host``) / ``172.17.0.1:3000`` (see docker bridge ip via ``ifconfig``)
     - Assuming the API is running on port ``3000``
   * - **API (outside Docker) to Hasura** 
     - ``localhost:8080``
     - ``localhost:8080``
     - ``localhost:8080``
     - Hasura runs on port ``8080`` by default
   * - **Hasura to API (both in docker-compose)** 
     - service name, e.g.: ``api:3000``
     - service name, e.g.: ``api:3000``
     - service name, e.g.: ``api:3000``
     - Assuming the API is running on port ``3000``
   * - **API to Hasura (both in docker-compose)** 
     - service name, e.g.: ``hasura:8080``
     - service name, e.g.: ``hasura:8080``
     - service name, e.g.: ``hasura:8080``
     - Hasura runs on port ``8080`` by default
   * - **Hasura to API (both running with separate docker run)** 
     - Docker internal IP address 
     - Docker internal IP address 
     - Docker internal IP address 
     - Can be obtained with ``docker inspect``
   * - **API to Hasura (both running with separate docker run)** 
     - Docker internal IP address 
     - Docker internal IP address 
     - Docker internal IP address 
     - Can be obtained with ``docker inspect``

.. note::

  Learn more about Docker specific networking in the `Docker documentation <https://docs.docker.com/network/>`__.
