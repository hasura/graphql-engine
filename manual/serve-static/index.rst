.. meta::
   :description: How to serve static files using hasura
   :keywords: hasura, manual, static files, custom service, nginx

====================
Serving static files
====================

You might want to serve static files like ``html/css/js/images`` via a static file service on hasura.

This can be achieved by setting up an nginx service on hasura which will serve these files.
For this you should create a git-push based custom service on Hasura using Hasura's ``nginx quickstart template``.

Follow the steps given at :ref:`deploy-webapp` and choose the ``nginx`` template to deploy your static file service.