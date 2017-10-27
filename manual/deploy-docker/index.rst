.. meta::
   :description: How to deploy docker images using hasura
   :keywords: hasura, manual, docker, image, custom service

===========================================
Deploying custom services using Docker images
===========================================

Not all requirements for your app may be met by the Hasura data, auth APIs.
Custom APIs and services will always need to be added specifically to the project.

If the service needed to be run has a ``docker`` image, Hasura provides an easy
way to deploy the service by simply specifying the ``docker`` image.

Example: Adding a custom database browser (adminer)
---------------------------------------------------

The adminer docker image is available as
`clue/adminer <https://hub.docker.com/r/clue/adminer/>`_.

Create a new service
^^^^^^^^^^^^^^^^^^^^

.. code-block:: shell

  $ hasura service add adminer

This will create a new directory called ``adminer`` in the ``services``
directory with Kubernetes specs.

Edit the ``services/adminer/k8s.yaml`` file
1. Under ``containers`` edit the ``image`` name, change
   ``image: hasura/hello-world:latest`` to ``image: clue/adminer``
2. Change the ``containerPort`` to ``80``

Create a route for the service
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now to expose the above created service, we have to create a route for it.

.. code-block:: shell

  $ hasura route generate adminer


Push this newly created service to the cluster
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Apply the service configuration by running:

.. code-block:: shell

  $ hasura push

And this should deploy the adminer service to your cluster.

If you head to ``https://adminer.<project-name>.hasura-app.io`` you'll see the
familiar ``adminer`` UI.

.. admonition:: Automatic SSL certificates

   The Hasura platform automatically creates Grade A SSL certificates using LetsEncrypt.

   SSL certificate creation can take a few minutes. During this time ``https://adminer.<project-name>.hasura-app.io``
   will not served, and you'll have to access your service on ``http`` instead. As soon as
   the certificate is ready, ``http://adminer.<project-name>.hasura-app.io`` will automatically
   start redirecting to the ``https`` version.
