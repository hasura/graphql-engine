.. .. meta::
   :description: How to deploy docker images using hasura
   :keywords: hasura, manual, docker, image, custom microservice


Deploying custom microservices using Docker images
==================================================
Not all requirements for your app may be met by the Hasura data, auth APIs.
Custom APIs and microservices will always need to be added specifically to the
project.

If the microservice needed to be run has a ``docker`` image, Hasura provides an
easy way to deploy it by simply specifying the ``docker`` image.

Example: Adding a custom blog microservice (ghost)
--------------------------------------------------
Let's say we want to deploy a custom blog service using `Ghost
<https://ghost.org>`_.

The ghost docker image is available as ``ghost`` on Docker hub
(https://hub.docker.com/_/ghost/).

First, make sure you already have a Hasura project, and a cluster added to it.
If not, quickstart the ``base`` project from `Hasura hub
<https://hasura.io/hub>`_.

Create a new microservice
-------------------------

.. code-block:: shell

  $ hasura microservice generate blog --image ghost --port 2368

This will create a new directory called ``blog`` in the ``microservices``
directory with Kubernetes specs (where the docker image and port is set
according to the above specified flags).

Create a route for the microservice
-----------------------------------
Now to expose our ``blog`` microservice externally, we have to create a route
for it.

.. code-block:: shell

  $ hasura conf generate-route blog >> conf/routes.yaml

This will generate a route for the microservice and append it to
``conf/routes.yaml``.

Push this newly created microservice to the cluster
---------------------------------------------------
Until now we made all the changes (adding a new microservice and it's routes)
inside our project directory. We have to now apply all these changes to our
cluster.

So let's commit and push your project to the cluster. Use ``git commit`` to
commit the changes, and then ``push``:

.. code-block:: bash

   # in the root of your project directory
   $ git add .
   $ git commit -am "Added blog"
   $ git push hasura master

This will deploy the ``blog`` microservice and the route configuration to your
cluster.

If you head to ``https://blog.<cluster-name>.hasura-app.io`` you'll see the
ghost blog UI.


.. admonition:: Automatic SSL certificates

   The Hasura platform automatically creates Grade A SSL certificates using LetsEncrypt.

   SSL certificate creation can take a few minutes. During this time ``https://adminer.<cluster-name>.hasura-app.io``
   will not served, and you'll have to access your microservice on ``http`` instead. As soon as
   the certificate is ready, ``http://adminer.<cluster-name>.hasura-app.io`` will automatically
   start redirecting to the ``https`` version.

