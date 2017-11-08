.. .. meta::
   :description: How to serve static files using hasura
   :keywords: hasura, manual, static files, custom microservice, nginx

====================
Hosting static files
====================

You might want to serve static files like ``html/css/js/images`` via a static
file microservice on hasura.

This can be achieved by setting up an ``nginx`` based microservice on Hasura
which will serve these files.  For this you can create a git-push based custom
microservice using Hasura's quickstart templates.

First, make sure you already have a Hasura project.  If not, quickstart the
base project from `hasura hub <https://hasura.io/hub>`_

Generate the microservice
-------------------------

.. code-block:: bash

	$ hasura microservice generate mymicroservice --template=nginx

Generate a route
----------------
To expose this microservice externally, we have to generate a route for it:

.. code-block:: shell

	$ hasura conf generate-route mymicroservice >> conf/routes.yaml

Generate a remote
-----------------
To make this microservice a git-push microservice, we have to generate a remote for it:

.. code-block:: shell

  $ hasura conf generate-remote mymicroservice >> conf/ci.yaml


This will also append the microservice's route and remote configuration to the
conf files.

Add your static files to ``microservice/mymicroservice/app/src``. Finally, push
your changes to your cluster.

.. code-block:: shell

	$ git add .
	$ git commit -m "Added static files"
	$ git push hasura master


In case of changes, simply keep adding or removing your static files to and
from the ``app/src`` folder of the mymicroservice directory and push your
changes again to serve them.
