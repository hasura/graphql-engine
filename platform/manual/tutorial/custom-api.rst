Part XI: Custom code and microservices
======================================

Not all requirements for an app will be met by the Hasura APIs.
Custom APIs and microservices like API integrations or UI microservices will always
be needed to be added specifically to the project.

Hasura provides easy ways to deploy custom microservices by either specifying ``docker`` images
or by directly pushing code via ``git``.

Let us explore the 2 use cases:

Custom code: Adding a simple web-server to serve a UI
-----------------------------------------------------

We'll use a Nodejs Express microservice for the server. To do this, we'll use the ``api`` microservice from the
`hello-nodejs-express <https://hasura.io/hub/projects/hasura/hello-nodejs-express>`_ hub project as the base (it is
basically a nodejs-express microservice boilerplate).

Initialize the microservice using:

.. code-block:: bash

   # in the project directory
   $ hasura microservice clone api --from hasura/hello-nodejs-express

This will create a directory called ``api`` inside your ``microservices`` directory, which  will contain a ``k8s.yaml``
file, a ``Dockerfile`` and the source code for the Nodejs Express server.

The ``k8s.yaml`` file contains the Kubernetes specs for the microservice. The ``Dockerfile`` is used by the Hasura platform
to automatically build your code in the right environment.

Now let us modify the ``/microservices/api/src/server.js`` file to just serve one sample request. Update the contents of
the file with:

.. snippet:: javascript
   :filename: server.js

   var express = require('express');
   var app = express();

   app.get('/', function (req, res) {
     console.log('Request received');
     res.send('Hello! The api microservice is now deployed!');
   });

   app.listen(8080, function () {
     console.log('Example app listening on port 8080!');
   });


Add a route to ``conf/routes.yaml`` so that the microservice can be reached externally:

.. code-block:: bash

    $ hasura conf generate-route api >> conf/routes.yaml

Generate and add a remote to ``conf/ci.yaml`` to add continuous integration (deploy code using git-push):

.. code-block:: bash

    $ hasura conf generate-remote api >> conf/ci.yaml

Once this is done, we're ready to push!

.. code-block:: bash

   # in project directory
   $ git add .
   $ git commit -m "added api microservice"
   $ git push hasura master

Voila, our microservice is deployed and live at ``https://api.<cluster-name>.hasura-app.io``! In case there are any
errors in building or deploying our code, the ``git push`` command will show you errors and the push will fail.
You'll have to fix the errors, commit and push again!

You can add more code to the above nodejs-express app to build a proper UI and then simply commit and ``git push`` to redeploy.

.. admonition:: Behind The Scenes

   The Hasura platform basically builds a ``docker`` image from the latest git changes
   pushed by you, and deploys the corresponding ``kubernetes`` microservice, deployment underneath.


Docker: Adding a custom database browser (adminer)
--------------------------------------------------

You can also add microservices from Docker images. For example, let's try to generate an adminer microservice using
`clue/adminer <https://hub.docker.com/r/clue/adminer/>`_.

.. code-block:: bash

   # in project directory
   $ hasura microservice generate adminer --image clue/adminer --port 80

This will create a directory inside the ``microservices`` directory called ``adminer`` which will contain a
``k8s.yaml`` file. This file describes the Kubernetes configuration for our microservice including the docker
image details.

Next, to expose this service externally, generate a route and add it to ``conf/routes.yaml``

.. code-block:: bash

	  $ hasura conf generate-route adminer >> conf/routes.yaml

Once this is done, we're ready to push!

.. code-block:: bash

   # in project directory
   $ git add .
   $ git commit -m "added adminer microservice"
   $ git push hasura master

That's all you need to do. If you head to ``https://adminer.<cluster-name>.hasura-app.io`` you'll see
the familiar ``adminer`` UI.

.. admonition:: Automatic SSL certificates

   The Hasura platform automatically creates Grade A SSL certificates using LetsEncrypt.

   SSL certificate creation can take a few minutes. During this time ``https://adminer.<cluster-name>.hasura-app.io``
   will not be served, and you'll have to access your microservice on ``http`` instead. As soon as
   the certificate is ready, ``http://adminer.test42.hasura-app.io`` will automatically
   start redirecting to the ``https`` version.

Next: Using the Auth UI Kit
---------------------------

Next, head to :doc:`ui-kit`.
