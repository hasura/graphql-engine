.. _deploy-host-webapps:

Adding a microservice from a template
=====================================

If you're starting with a new project head to `hasura.io/hub <https://hasura.io/hub>`_
and get started with a
new hasura project that already has the boilerplate for your stack.

Use this guide if you are adding a microservice to your existing Hasura project.

Microservice stack templates
----------------------------

We maintain a list of `stack templates <https://github.com/hasura/microservice-templates>`_
so that it's easy for you to add a microservice boilerplate to
your project. Every template consists of:

1. A ``Dockerfile``
2. Source code
3. A ``k8s.yaml`` file

The Dockerfile defines how the source code gets built, and the ``k8s.yaml`` is the kubernetes spec
file that defines how this microservice gets deployed. You can go edit the ``Dockerfile`` and the
``k8s.yaml`` as you want if you know what you're doing. However, you don't have to know how docker
and kubernetes work to get started.

Step 1: Find your template name
-------------------------------

Run:

.. code-block:: bash

   $ hasura microservice template-list
   • Updating templates...

   csharp-aspnet
   docker
   elasticsearch
   go-gin
   go-iris
   go-raw
   go-simple-server
   haskell-spock
   java-play
   java-spark
   java-spring-boot
   js-angularjs
   mysql
   nginx
   nodejs-express
   nodejs-express-sass
   php-apache
   php-laravel
   python-cron
   python-flask
   r-shiny
   react
   schemaspy
   swift-perfect
   swift-vapor
   typescript-angular

If you're choosing a template from any of the above, continue, otherwise you'll have to create a ``Dockerfile`` and a ``k8s.yaml`` file yourself.

To create the files yourself, head to :doc:`Add a custom Dockerfile <using-custom-dockerfile>`.
Otherwise, continue with Step 2 below!

Step 2: Add the microservice template to your microservices folder
------------------------------------------------------------------

Let's say you want to use ``nodejs-express`` and you name the microservice ``my-app``. Now run:

.. code-block:: bash

   $ hasura ms create my-app --template=nodejs-express

This will create the following in your projects directory:

.. code-block:: bash

   ├── microservices/
       └── my-app/              # a new folder to contain microservice code/config
           ├── k8s.yaml
           ├── Dockerfile
           └── app/

The exact folder names might be different depending on what you do, however the ``k8s.yaml`` file and the ``Dockerfile`` will always be there.

Step 3: Tell the hasura CLI that you want to git-push to deploy this microservice
---------------------------------------------------------------------------------

Add configuration to your ``conf/ci.yaml`` file so that ``git push hasura master`` will
automatically deploy your source code, build the docker image, and rollout the update!

.. code:: bash

   $ hasura conf generate-remote my-service >> conf/ci.yaml

.. admonition:: Behind The Scenes

   Checkout :ref:`ci.yaml <hasura-dir-conf-ci.yaml>` to learn more about this file 

Step 4: Optional: Expose this microservice to the world
-------------------------------------------------------

Let's expose this microservice to the external world on the subdomain ``my-app``.
The configuration for routes for all the microservices on your Hasura project
are configured in ``conf/routes.yaml``.
The ``hasura`` CLI provides a handy command to generate the
default routes configuration for your custom microservice:

.. code:: bash

    $ hasura conf generate-route my-service >> conf/routes.yaml

This command above will add the default route configuration for your microservice to the ``conf/routes.yaml`` file.

.. admonition:: Behind The Scenes

   Checkout :ref:`routes.yaml <hasura-dir-conf-routes.yaml>` to learn more about this file 

Step 5: Git push and deploy!
----------------------------

.. code:: bash

    $ git add microservices/my-app
    $ git add conf/ci.yaml
    $ git add conf/routes.yaml
    $ git commit -am 'Adds my-app microservice boilerplate, ci and route config'
    $ git push hasura master

That's it! And you'll have a shiny new microservice deployed to your cluster.

Check out the running microservices:

.. code:: bash

   $ hasura microservices list

    INFO Custom microservices:
    NAME          STATUS    URL
    my-app        Running   https://my-app.cluster-name.hasura-app.io


Open the microservice in your browser:

.. code:: bash

   $ hasura microservices open my-app

