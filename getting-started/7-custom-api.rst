================================================
Part VII: Custom code using Docker or `git push`
================================================

Not all requirements will be met by the Hasura data, auth APIs.
Custom APIs and services like, API integrations or UI services will always
need to be added specifically to the project.

Hasura provides an easy way to run services by specifying ``docker`` images
or by pushing changes from your git repo, directly onto your project.

Let us explore 2 use cases.

.. admonition:: Video reference

   `Adding a custom service using a Docker image, or by using git push <https://youtu.be/LK1mgsl2uUs>`_


Docker: Adding a custom database browser (adminer)
--------------------------------------------------

To add a custom service, head to the console, and click on the ``+`` icon.
Follow the instructions from this screenshot, and click on ``Create`` to add your service.

.. image:: adminer.png
   :scale: 50%

That's all you need to do. If you head to ``https://adminer.test42.hasura-app.io`` you'll see
the familiar ``adminer`` UI.

.. admonition:: Automatic SSL certificates

   The Hasura platform automatically creates Grade A SSL certificates using LetsEncrypt.

   SSL certificate creation can take a few minutes. During this time ``https://adminer.test42.hasura-app.io``
   will not served, and you'll have to access your service on ``http`` instead. As soon as
   the certificate is ready, ``http://adminer.test42.hasura-app.io`` will automatically
   start redirecting to the ``https`` version.


Git push: Adding a simple web-server to serve a UI
--------------------------------------------------

Let's take a simple nodejs, express example. The best way to get a base setup ready, is to
grab the relevant base template directory from `git-push-templates <https://github.com/hasura/git-push-templates>`_

Let's use the ``git-push-templates/nodejs-express`` for our example, and create a directory called ``www``.

After getting the relevant directory, and intializing our own git repo in it, this is what
our directory structure should look like::

   www/
      Dockerfile
      .git
      app/
         package.json
         server.js

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


Now that our code is ready to be consumed by the Hasura project, let's create a new service via the console.
To enable this option to ``git push`` a service, check the ``Enable git push`` box on the ``Add a custom service`` page.

.. image:: gitpush.png
   :scale: 50%

Make sure that you've added your SSH public key to the ``authorized_keys`` file via the ``Console > Advanced`` page.

Once that is done, you're ready to push!

.. code-block:: console

   $ git push hasura master

Voila, your service is deployed and live! In case there are any errors in building or deploying your code,
the ``git push`` command will show you errors and the push will fail. Fix the error, and push again!

.. admonition:: Behind The Scenes

   The Hasura platform basically builds a docker image from the latest git changes
   pushed by you, and deploys the right kubernetes service, deployment underneath.

   If you want finer control over your deployment, you are encouraged to use ``kubectl``
   and peek under the hood of the service that is automatically deployed.
