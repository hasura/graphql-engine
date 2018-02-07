.. .. meta::
   :description: How to deploy docker images using hasura
   :keywords: hasura, manual, docker, image, custom microservice

.. _hasura_microservice_doc:

=========================================
Developing & hosting custom microservices
=========================================

A Hasura project is composed of a set of microservices.
These include certain Hasura microservices but can also
include your own microservices.

Some typical examples of microservices you would add to your application include, an API service,
a webapp, a microservice that serves static files, for a simple HTML site or a SPA (eg: React),
a ready made microservice like the ghost blog-engine or a 'stateful' microservice like a database.

**NOTE**: If you're starting a new hasura project, head to `hasura.io/hub <https://hasura.io/hub>`_ to
find the right stack boilerplate and follow the guide there!

If you're thinking about adding a new microservice to your existing Hasura project, pick up one of the following guides:

* :doc:`Use a template for your stack <add-microservice-from-template>`:

  - Ideal for webapps, APIs, static file-hosting
  - Use this if you don't know how to use a Dockerfile

* :doc:`Use your own Dockerfile and source-code <add-custom-dockerfile>`
* :doc:`Use an existing docker image <add-docker-image>`

Here's what your Hasura cluster looks like before and after you add custom microservices:

.. image:: adding-custom-microservices.png

These are the 3 key steps you need to follow for deploying any microservice:

1. Create a folder inside the ``microservices`` directory in your project.
2. Configure ``git push hasura master`` to build and deploy your microservice.
3. Expose the microservice externally to the world by adding a route (subdomain, or path).

This is what your project folder might look like once you've added a microservice:

.. code:: bash

   ├── microservices/
       └── app/              # a new folder to contain microservice code/config
           ├── k8s.yaml
           ├── Dockerfile
           └── src/
   └── conf/
       ├── routes.yaml       # a new route entry in this file
       └── ci.yaml           # a new entry here to enable git-push

Now that you're familiar with the basic concepts
of what a microservice is, pick up one of the following
guides and get started:

.. toctree::
  :maxdepth: 1
  :titlesonly:

  Use a template for your stack <add-microservice-from-template>
  Use your own Dockerfile <add-custom-dockerfile>
  Using an existing docker image <add-docker-image>
  Debugging <debugging/index>
  Communicating between microservices <communicating-between-microservices>
  Connecting to PostgreSQL from microservices <connecting-to-postgres-from-microservice>
