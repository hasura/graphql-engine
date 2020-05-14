.. meta::
   :description: Network from and to Hasura
   :keywords: hasura, docs, deployment, network

.. _networking:

Network config
==============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Sometimes we need to connect from Hasura to an external API or vice versa. 
This page gives an overview of how you can do so.

Networking from / to Hasura
---------------------------

.. list-table:: 
   :stub-columns: 1
   :header-rows: 1

   * - 
     - Mac
     - Windows
     - Linux
   * - **Hasura to API**
     - ``host.docker.internal``
     - ``docker.for.win.localhost``
     - Add the ``--net=host`` flag 
   * - **API to Hasura** (with ``docker-compose``)
     - host name (*)
     - host name (*)
     - host name (*)

`*` You can run ``docker.inspect`` to get the IP address (host name). 

.. note::

  Learn more about container networking in the `Docker documentation <https://docs.docker.com/network/>`__.
