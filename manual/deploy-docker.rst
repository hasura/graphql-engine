.. meta::
   :description: How to deploy docker images using hasura
   :keywords: hasura, manual, docker, image, custom service

===========================================
Deploy custom services using Docker images
===========================================

Not all requirements for your app may be met by the Hasura data, auth APIs.
Custom APIs and services will always need to be added specifically to the project.

If the service needed to be run has a ``docker`` image Hasura provides an easy way to deploy the service by simply specifying the ``docker`` image.


.. admonition:: Video reference

   `Adding a custom service using a Docker image <https://youtu.be/LK1mgsl2uUs>`_


**Example: Adding a custom database browser (adminer) using its Docker image**

The adminer docker image is available as `clue/adminer <https://hub.docker.com/r/clue/adminer/>`_.

To add a new custom service, head to the console, and click on the ``+`` icon.
Follow the instructions from this screenshot, and click on ``Create`` to add your service.

.. image:: adminer.png
   :scale: 50%

That's all you need to do. If you head to ``https://adminer.<project-name>.hasura-app.io`` you'll see
the familiar ``adminer`` UI.

.. admonition:: Automatic SSL certificates

   The Hasura platform automatically creates Grade A SSL certificates using LetsEncrypt.

   SSL certificate creation can take a few minutes. During this time ``https://adminer.<project-name>.hasura-app.io``
   will not served, and you'll have to access your service on ``http`` instead. As soon as
   the certificate is ready, ``http://adminer.<project-name>.hasura-app.io`` will automatically
   start redirecting to the ``https`` version.