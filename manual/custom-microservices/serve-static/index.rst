.. .. meta::
   :description: How to serve static files using hasura
   :keywords: hasura, manual, static files, custom microservice, nginx

====================
Hosting static files
====================

You might want to serve static files like ``html/css/js/images`` via a static file microservice on hasura.

This can be achieved by setting up an ``nginx`` microservice on Hasura which will serve these files.
For this you can create a git-push based custom microservice using Hasura's ``quickstart templates``.

Follow the steps given at :ref:`deploy-host-webapps` and choose the ``nginx`` template to deploy your static file microservice.

Then simply keep adding your static files to the ``app/src`` folder of the nginx microservice code and redeploy to serve them.
