.. .. meta::
   :description: How to deploy docker images using hasura
   :keywords: hasura, manual, docker, image, custom microservice

.. _hasura_microservice_doc:

=========================================
Developing & hosting custom microservices
=========================================

A Hasura project is composed of a set of microservices.
These include certain Hasura microservices like, ``postgres``, ``nginx``, ``data API``, ``auth API`` and more but can also
include your own microservices.

Some typical examples of microservices you would add to your application:

1. A custom API written in python-flask
2. A webapp that serves a UI written with the express framework in nodejs
3. A microservice that serves static files, for a simple HTML site, or a SPA in React or Angular
4. A ready made microservice like the ghost blog-engine
5. A 'stateful' microservice like a database

Hasura makes it easy for you to build and deploy custom microservices.

Here's what your Hasura cluster looks like before and after you add custom microservices:

.. image:: adding-custom-microservices.png

These are the 3 key steps you need to follow for deploying any microservice:

1. Generate a microservice inside your project, using ``hasura generate
   microservice``. This will create a directory with the microservice name
   inside the ``microservice`` directory.  This will contain Kuberenetes specs
   for the microservice, optionally it can contain the source code and
   Dockerfile too.

2. Add a route (subdomain, or path) on which this microservice may be exposed
   to the external world.

3. Optionally, add it to the ``hasura`` remote, so that the microservice is
   deployed whenever you ``git push``.


.. .. todo::
   Recommended sections/pages in this section:
   1. How custom microservices work on a Hasura cluster
   2. Developing & hosting webapps:
      - Quickstart
      - Dockerfile, directory setup, git push
      - Contacting internal microservices
      - Using session middleware
      - local-development
      - Get logs
   3. Developing & hosting APIs
      - Repeats as in 2
   4. Developing & hosting static files
      - Repeats as in 2
   5. Developing & hosting docker containers
      - Repeats as in 2
   6. Routing
   7. Configuring git-push
   8. Adding persistent storage
   9. Monitoring and logs
   10. Deploying non-HTTP microservices
   11. Deploying kubernetes objects
  TOCTREE:
  Communicating between microservices <communicating-between-microservices>
  Hosting webapps
  Hosting APIs
  Hosting static files
  develop-custom-services/index
  Hosting Docker containers <deploy-docker/index>
  Routing <routing>
  serve-static/index
  persistent-services
  deploy-non-http
  deploy-k8s-objects

Read in details about the various ways to develop and deploy custom
microservices.

.. toctree::
  :maxdepth: 1
  :titlesonly:

  Communicating between microservices <communicating-between-microservices>
  develop-custom-services/index
  Hosting Docker containers <deploy-docker/index>
  serve-static/index
