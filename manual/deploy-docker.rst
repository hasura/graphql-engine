.. meta::
   :description: How to deploy docker images using hasura
   :keywords: hasura, manual, docker, image, custom service

===========================================
Deploy custom services using Docker images
===========================================

Not all requirements for your app may be met by the Hasura data, auth APIs.
Custom APIs and services will always need to be added specifically to the project.

If the service needed to be run has a ``docker`` image, Hasura provides an easy way to deploy the service by simply specifying the ``docker`` image.

**Example: Adding a custom database browser (adminer) using its Docker image**

The adminer docker image is available as `clue/adminer <https://hub.docker.com/r/clue/adminer/>`_.

| Docker based services are created using the project console.
| Navigate to: ``Project Console -> Custom Microservice -> Docker -> Create New.``

Here is a reference image for the page we land on:

.. image:: deploy-docker.png
   :scale: 50%


Just add the name of the service (say adminer), the docker image (viz. clue/adminer) and hit ``Create``. That's all you need to do.
If you head to ``https://adminer.<project-name>.hasura-app.io`` you'll see the familiar ``adminer`` UI.

.. admonition:: Automatic SSL certificates

   The Hasura platform automatically creates Grade A SSL certificates using LetsEncrypt.

   SSL certificate creation can take a few minutes. During this time ``https://adminer.<project-name>.hasura-app.io``
   will not served, and you'll have to access your service on ``http`` instead. As soon as
   the certificate is ready, ``http://adminer.<project-name>.hasura-app.io`` will automatically
   start redirecting to the ``https`` version.