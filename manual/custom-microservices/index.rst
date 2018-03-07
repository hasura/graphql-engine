.. .. meta::
   :description: How to deploy docker images using hasura
   :keywords: hasura, manual, docker, image, custom microservice

.. _hasura_microservice_doc:

Running custom microservices
============================

A Hasura project is composed of a set of microservices.
These include certain Hasura microservices but can also
include your own microservices.

Some typical examples of microservices you would add to your application include, an API service,
a webapp, a microservice that serves static files, for a simple HTML site or a SPA (eg: React),
a ready made microservice like the ghost blog-engine or a 'stateful' microservice like a database.

.. note::

    If you're starting a new hasura project, head to `hasura.io/hub <https://hasura.io/hub>`_ to
    find the right starting point project and follow the guide there!

Here's what your Hasura cluster looks like before and after you add custom microservices:

.. image:: adding-custom-microservices.png

Now that you're familiar with the basic concepts
of what a microservice is, See:

.. toctree::
  :maxdepth: 1
  :titlesonly:

  Adding microservices <add-microservice/index>
  debugging/index
  communicating-between-microservices>
  Connecting to PostgreSQL from microservices <connect-postgres>
