.. .. meta::
   :description: How to serve static files using hasura
   :keywords: hasura, manual, static files, custom service, nginx

====================
Hosting static files
====================

You might want to serve static files like ``html/css/js/images`` via a static file service on hasura.

This can be achieved by setting up an ``nginx`` service on Hasura which will serve these files.
For this you can create a git-push based custom service using Hasura's ``quickstart templates``.

Firstly, quickstart base from `hasura hub <https://hasura.io/hub>`_

.. code-block:: shell
	$ hasura quickstart base && cd base

Now generate the microservice.

.. code-block:: shell
	$ hasura microservice generate mymicroservice --template=nginx

Generate a remote and route to this microservice

.. code-block:: shell
	$ hasura conf generate-route mymicroservice >> conf/routes.yaml
  	$ hasura conf generate-remote mymicroservice >> conf/ci.yaml

This will also append the microservice to the conf files.

Add your static files to ``microservice/mymicroservice/app/src``. Finally, push your changes to your cluster.

.. code-block:: shell
	$ git add .
	$ git commit -m "Added static files"
	$ git push hasura master


In case of changes, simply keep adding or removing your static files to and from the ``app/src`` folder of the mymicroservice directory and push your changes again to serve them.
