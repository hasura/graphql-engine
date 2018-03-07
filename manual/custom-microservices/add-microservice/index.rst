.. .. meta::
   :description: How to deploy docker images using hasura
   :keywords: hasura, manual, docker, image, custom microservice

.. _hasura_microservice_doc:

Adding custom microservices
===========================

If you're thinking about adding a new microservice to your existing Hasura project, pick up one of the following guides:

* :doc:`Using a template <using-template>`:

  - Ideal for webapps, APIs, static file-hosting
  - Use this if you don't know how to use a Dockerfile

* :doc:`Using your own Dockerfile and source-code <using-custom-dockerfile>`
* :doc:`Using an existing Docker image <using-docker-image>`

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


.. toctree::
  :maxdepth: 1
  :hidden:

  Using a template <using-template>
  Using your own Dockerfile and source-code <using-custom-dockerfile>
  Using an existing Docker image <using-docker-image>
