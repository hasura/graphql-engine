.. .. meta::
   :description: How to deploy docker images using hasura
   :keywords: hasura, manual, docker, image, custom microservice

===========================================
Deploying custom microservices using Docker images
===========================================

Not all requirements for your app may be met by the Hasura data, auth APIs.
Custom APIs and microservices will always need to be added specifically to the project.

If the microservice needed to be run has a ``docker`` image, Hasura provides an easy
way to deploy the microservice by simply specifying the ``docker`` image.

Example: Adding a custom database browser (adminer)
---------------------------------------------------

The adminer docker image is available as
`clue/adminer <https://hub.docker.com/r/clue/adminer/>`_.

Create a new microservice
^^^^^^^^^^^^^^^^^^^^

.. code-block:: shell

  $ hasura microservice add adminer

This will create a new directory called ``adminer`` in the ``services``
directory with Kubernetes specs.

Edit the ``services/adminer/k8s.yaml`` file:

1. Under ``containers`` edit the ``image`` name to ``image: clue/adminer``

2. Change the ``containerPort`` to ``80``

Create a route for the microservice
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now to expose the above created microservice, we have to create a route for it.

.. code-block:: shell

  $ hasura route generate adminer


Push this newly created microservice to the cluster
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Apply the microservice configuration by running:

.. code-block:: shell

  $ hasura push

And this should deploy the adminer microservice to your cluster.

If you head to ``https://adminer.<cluster-name>.hasura-app.io`` you'll see the
familiar ``adminer`` UI.

.. admonition:: Automatic SSL certificates

   The Hasura platform automatically creates Grade A SSL certificates using LetsEncrypt.

   SSL certificate creation can take a few minutes. During this time ``https://adminer.<cluster-name>.hasura-app.io``
   will not served, and you'll have to access your microservice on ``http`` instead. As soon as
   the certificate is ready, ``http://adminer.<cluster-name>.hasura-app.io`` will automatically
   start redirecting to the ``https`` version.
