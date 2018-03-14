.. .. meta::
   :description: How to deploy docker images using hasura
   :keywords: hasura, manual, docker, image, custom microservice


Adding a microservice using an existing Docker image
====================================================

Follow this manual if you already have a ``docker`` image,
and want to run it as a microservice on
your Hasura cluster.

Example: Adding a custom blog microservice (ghost)
--------------------------------------------------
Let's say we want to deploy a custom blog service using `Ghost
<https://ghost.org>`_.

The ghost docker image is available as ``ghost:latest`` on Docker hub
(https://hub.docker.com/_/ghost/).

First, make sure you already have a Hasura project.

Step 1: Create a new microservice
---------------------------------

Let's say you name the microservice ``<my-blog>``. Run the following command:

.. code-block:: shell

  $ hasura microservice create <my-blog> --image ghost:latest --port 2368

This will create a new directory called ``<my-blog>`` in the ``microservices``
directory with Kubernetes specs file ``k8s.yaml``:

.. code-block:: bash

   ├── microservices/
       └── <my-blog>/
           └── k8s.yaml

Step 2: Create a route for the microservice
--------------------------------------------
Now to expose our ``<my-blog>`` microservice externally, we have to create a route
for it.

.. code-block:: bash

  $ hasura conf generate-route <my-blog> >> conf/routes.yaml

This will generate a route for the microservice and append it to
``conf/routes.yaml``.

.. admonition:: Behind The Scenes

   Checkout :ref:`routes.yaml <hasura-dir-conf-routes.yaml>` to learn more about this file 

Step 3: Git push and deploy!
----------------------------

.. code:: bash

    $ git add microservices/<my-blog>
    $ git add conf/routes.yaml
    $ git commit -am 'Adds ghost docker image, and route config'
    $ git push hasura master

That's it! And you'll have a shiny new microservice deployed to your cluster.

Check out the running microservices:

.. code:: bash

   $ hasura microservices list

    INFO Custom microservices:
    NAME          STATUS    URL
    my-blog          Running   https://my-blog.cluster-name.hasura-app.io


Open the microservice in your browser:

.. code:: bash

   $ hasura microservices open <my-blog>
