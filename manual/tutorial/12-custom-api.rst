.. meta::
   :description: Part 5 of a set of learning exercises meant for exploring Hasura in detail. This part shows you how to create a custom microservice (Docker & git push)
   :keywords: hasura, getting started, step 7, custom microservice, Docker, git push

=======================================
Part XII: Custom code and microservices
=======================================

.. todo::

   Update entire section with the new hasura CLI commands

Not all requirements will be met by the Hasura APIs.
Custom APIs and microservices like API integrations or UI microservices will always
need to be added specifically to the project.

Hasura provides an easy way to run microservices by specifying ``docker`` images
or by directly pushing code via ``git``.

Let us explore 2 use cases.

Custom code: Adding a simple web-server to serve a UI
-----------------------------------------------------

Initialize the microservice configuration (we'll call it ``www``) using:

.. code-block:: console
		
	  $ hasura microservice add www

This will create a directory called ``www`` inside your ``microservices`` directory, which  will contain a ``k8s.yaml`` file. This file contains the Kubernetes configuration for the microservice. 

.. todo::

   Rewrite this section with better instructions 

Now coming to the code, let's take a simple nodejs express example. The best way to get a base setup ready, is to
grab the relevant base template directory from `quickstart-docker-git <https://github.com/hasura/quickstart-docker-git>`_

Let's copy the ``quickstart-docker-git/nodejs-express`` inside the ``www`` directory.

Note the ``Dockerfile`` at the top level. This Dockerfile is used by the Hasura platform
automatically to build your code in the right environment.

Let's modify ``www/app/server.js``, to just serve one sample request:

.. snippet:: javascript
   :filename: server.js

   var express = require('express');
   var app = express();

   app.get('/', function (req, res) {
     console.log('Request received');
     res.send('Hello world!');
   });

   app.listen(8080, function () {
     console.log('Example app listening on port 8080!');
   });


Add a route so that the microservice can be reached externally:

.. code-block:: console
		
	  $ hasura routes generate www
	  
:: 

    INFO Generating route...                          
    INFO Add the following block to conf/routes.yaml  

    www:
      /:
	corsPolicy: null
	enableAuth: true
	enableCORS: true
	enableWebsockets: true
	locationDirectives: ""
	restrictToRoles: []
	upstreamService:
	  name: www
	  namespace: '{{ cluster.metadata.namespaces.user }}'
	upstreamServicePath: /
	upstreamServicePort: 80


Add the output above to the ``conf/routes.yaml`` as instructed.

If you want to have a separate git remote to push your code to, you can use the ``hasura remote generate`` command to do so.

Make sure that you've added your SSH public key to the cluster using

.. code-block:: console

	  $ hasura ssh-key add
	  
Once that is done, you're ready to push!

.. code-block:: console

   $ git push hasura master

Voila, your microservice is deployed and live! In case there are any errors in building or deploying your code,
the ``git push`` command will show you errors and the push will fail. Fix the error, and push again!

.. admonition:: Behind The Scenes

   The Hasura platform basically builds a docker image from the latest git changes
   pushed by you, and deploys the right kubernetes microservice, deployment underneath.

   If you want finer control over your deployment, you are encouraged to use ``kubectl``
   and peek under the hood of the microservice that is automatically deployed.

Docker: Adding a custom database browser (adminer)
--------------------------------------------------

To add a custom microservice, open your teminal and ``cd`` into your project directory. Execute:

.. code-block:: console

   $ hasura microservice add adminer -i clue/adminer -p 80

This will create a directory inside the *microservices* directory called *adminer* which will contain a ``k8s.yaml`` file.
This file describes the Kubenernetes configuration for your microservice. 

Next, generate the routes for this microservice:

.. code-block:: console
		
	  $ hasura routes generate adminer
	  
::

     INFO Generating route...                          
     INFO Add the following block to conf/routes.yaml  

     adminer:
       /:
	 corsPolicy: null
	 enableAuth: true
	 enableCORS: true
	 enableWebsockets: true
	 locationDirectives: ""
	 restrictToRoles: []
	 upstreamService:
	   name: adminer
	   namespace: '{{ cluster.metadata.namespaces.user }}'
	 upstreamServicePath: /
	 upstreamServicePort: 80


Add this to the ``conf/routes.yaml`` file as instructed in the output of the above command.

Finally run ``git push`` to deploy the configuration and microservices to the cluster.

That's all you need to do. If you head to ``https://adminer.<cluster-name>.hasura-app.io`` you'll see
the familiar ``adminer`` UI.

.. admonition:: Automatic SSL certificates

   The Hasura platform automatically creates Grade A SSL certificates using LetsEncrypt.

   SSL certificate creation can take a few minutes. During this time ``https://adminer.test42.hasura-app.io``
   will not served, and you'll have to access your microservice on ``http`` instead. As soon as
   the certificate is ready, ``http://adminer.test42.hasura-app.io`` will automatically
   start redirecting to the ``https`` version.



