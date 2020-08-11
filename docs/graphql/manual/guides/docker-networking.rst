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

Hasura to API (outside Docker)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Linux

    1. With ``--net=host``, e.g. ``localhost:3000 --net=host``

    2. Otherwise, ``<docker-bridge-ip>:3000``, e.g. ``172.17.0.1:3000`` 

    .. note::
    
      The Docker bridge IP can be found via ``ifconfig``.

  .. tab:: Mac

    ``host.docker.internal:3000``

  .. tab:: Windows

    ``docker.for.win.localhost:3000``

API (outside Docker) to Hasura
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Linux

    ``localhost:8080``

  .. tab:: Mac

    ``localhost:8080``

  .. tab:: Windows

    ``localhost:8080``

.. note::

  Hasura runs on port ``8080`` by default.

Hasura to API (both in docker-compose)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Linux

    Service name, e.g.: ``api:3000``

  .. tab:: Mac

    Service name, e.g.: ``api:3000``

  .. tab:: Windows

    Service name, e.g.: ``api:3000``

API to Hasura (both in docker-compose)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Linux

    Service name, e.g.: ``hasura:8080``

  .. tab:: Mac

    Service name, e.g.: ``hasura:8080``

  .. tab:: Windows
  
    Service name, e.g.: ``hasura:8080``

.. note::

  Hasura runs on port ``8080`` by default.

Hasura to API (both running with separate docker run)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Linux

    Docker internal IP address 

  .. tab:: Mac

    Docker internal IP address 

  .. tab:: Windows

    Docker internal IP address 

.. note::

  The Docker internal address can be obtained with ``docker inspect``.

API to Hasura (both running with separate docker run)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Linux

    Docker internal IP address 

  .. tab:: Mac

    Docker internal IP address 

  .. tab:: Windows

    Docker internal IP address 

.. note::

  The Docker internal address can be obtained with ``docker inspect``.

Advanced
--------

Learn more about Docker specific networking in the `Docker documentation <https://docs.docker.com/network/>`__.
