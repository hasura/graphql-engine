.. .. meta::
   :description: How to deploy microservices using hasura templates
   :keywords: hasura, manual, template, custom microservice


Adding a microservice from a template
=====================================

.. note::

   If you're starting with a new project head to `hasura.io/hub <https://hasura.io/hub>`_ and get started with a new hasura project that already has the boilerplate for your stack.

Use this guide if you are adding a microservice to your existing Hasura project.

Step 1: Choosing a microservice template from Hub
--------------------------------------------------

You can clone microservices from any of the projects on `hasura.io/hub <https://hasura.io/hub>`_ and use them as a template for your microservice.

Go to `hasura.io/hub <https://hasura.io/hub>`_ and find a boilerplate project you want to clone from. Most projects should have an ``Included microservices`` section which describes the microservices available in the project.  See `Popular hub microservices:`_

Step 2: Cloning the microservice to your project
------------------------------------------------

To add the microservice to your project, you can use the ``hasura microservice clone`` command from the project directory.

.. code-block:: bash

    $ hasura microservice clone [[microservice-1] [microservice-2]...] --from [hub-user/hub-project-name] [flags]

Examples:

.. code-block:: bash

    # Clone microservice 'app' from 'hasura/hello-python-flask':
    $ hasura microservice clone app --from hasura/hello-python-flask
    # Clone all microservices from 'hasura/hello-react'
    $ hasura microservice clone --from hasura/hello-react
    # Clone microservices 'api' and 'ui' from 'hasura/hello-react'
    $ hasura microservice clone api ui --from hasura/hello-react

Step 3: Tell the hasura CLI that you want to git-push to deploy this microservice
---------------------------------------------------------------------------------

Add configuration to your ``conf/ci.yaml`` file so that ``git push hasura master`` will
automatically deploy your source code, build the docker image, and rollout the update!

.. code:: bash

   $ hasura conf generate-remote <new-app> >> conf/ci.yaml

.. admonition:: Behind The Scenes

   Checkout :ref:`ci.yaml <hasura-dir-conf-ci.yaml>` to learn more about this file

Step 4: (Optional) Expose this microservice to the world
---------------------------------------------------------

Let's expose this microservice to the external world on the subdomain ``<new-app>``.
The configuration for routes for all the microservices on your Hasura project
are configured in ``conf/routes.yaml``.
The ``hasura`` CLI provides a handy command to generate the
default routes configuration for your custom microservice:

.. code:: bash

    $ hasura conf generate-route <new-app> >> conf/routes.yaml

This command above will add the default route configuration for your microservice to the ``conf/routes.yaml`` file.

.. admonition:: Behind The Scenes

   Checkout :ref:`routes.yaml <hasura-dir-conf-routes.yaml>` to learn more about this file

Step 5: Git push and deploy!
----------------------------

.. code:: bash

    $ git add . && git commit -m 'Added <new-app>'
    $ git push hasura master

That's it! And you'll have a shiny new microservice deployed to your cluster.

Check out the running microservices:

.. code:: bash

   $ hasura microservices list

Open the microservice in your browser:

.. code:: bash

   $ hasura microservices open <new-app>


Popular hub microservices:
^^^^^^^^^^^^^^^^^^^^^^^^^^

Here are a few popular microservices you can choose from:

- ``app`` from `hasura/hello-python-flask <https://hasura.io/hub/project/hasura/hello-python-flask>`_
- ``ui`` from `hasura/hello-react <https://hasura.io/hub/project/hasura/hello-react>`_
- ``api`` from `hasura/hello-nodejs-express <https://hasura.io/hub/project/hasura/hello-nodejs-express>`_
- ``api`` from `hasura/hello-php-apache <https://hasura.io/hub/project/hasura/hello-php-apache>`_
- ``app`` from `hasura/hello-python-django <https://hasura.io/hub/project/hasura/hello-python-django>`_
- ``www`` from `hasura/hello-java-springboot <https://hasura.io/hub/project/hasura/hello-java-springboot>`_
- ``www`` from `hasura/hello-nginx <https://hasura.io/hub/project/hasura/hello-nginx>`_
- ``app`` from `hasura/hello-golang-raw <https://hasura.io/hub/project/hasura/hello-golang-raw>`_
- ``app`` from `hasura/docker-base <https://hasura.io/hub/project/hasura/docker-base>`_
- ``www`` from `hasura/hello-angularjs <https://hasura.io/hub/project/hasura/hello-angularjs>`_
- ``api`` from `hasura/hello-golang-iris <https://hasura.io/hub/project/hasura/hello-golang-iris>`_
- ``www`` from `hasura/hello-r-shiny <https://hasura.io/hub/project/hasura/hello-r-shiny>`_
- ``api`` from `hasura/hello-java-spark <https://hasura.io/hub/project/hasura/hello-java-spark>`_
- ``app`` from `hasura/hello-ruby-rails <https://hasura.io/hub/project/hasura/hello-ruby-rails>`_
- ``app`` from `hasura/hello-ruby-sinatra <https://hasura.io/hub/project/hasura/hello-ruby-sinatra>`_
- ``www`` from `hasura/hello-csharp-aspnet <https://hasura.io/hub/project/hasura/hello-csharp-aspnet>`_
- ``api`` from `hasura/hello-swift-vapor <https://hasura.io/hub/project/hasura/hello-swift-vapor>`_
- ``api`` from `hasura/hello-swift-perfect <https://hasura.io/hub/project/hasura/hello-swift-perfect>`_

