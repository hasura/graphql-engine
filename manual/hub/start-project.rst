.. _hub_start:

Starting with a project from /hub
=================================

Head to `hasura.io/hub <https://hasura.io/hub>`_ and choose a project that you want to start with.

Here are some useful projects to start with:

1. ``hasura/hello-*``: Projects that are published by the ``hasura`` user are projects that we have published for different frameworks to help you get started. See `Starter hub projects:`_
2. ``hasura/base``: Use the base project to start off on a completely empty project. You can add your own microservices, schemas and configurations to this.


Option 1: Clone the project and deploy it to a new cluster
----------------------------------------------------------

First, :doc:`install <../install-hasura-cli>` the hasura CLI.

Then, run the following instructions on your terminal or command line:

.. code-block:: bash

   hasura quickstart <user/my-project>
   cd <my-project>
   git add . && git commit -m 'Initial commit'
   git push hasura master

Once your `git push` succeeds, everything is deployed to a new free Hasura cluster.

Now you can add/remove/modify:

1. microservices: In the ``microservices/`` directory
2. schema: Run ``hasura api-console`` and head to the ``Data`` tab. You can browse, modify the schema appropriately.
3. conf: Modify the appropriate files in the ``conf/`` directory.


Option 2: Clone the project without deploying it
------------------------------------------------

You might want to clone a project without deploying it for a few reasons:

1. You want to deploy it on a cluster you already have
2. You want to clone the source code of the project as reference

First, :doc:`install <../install-hasura-cli>` the hasura CLI.

Then, run the following instructions on your terminal or command line:

.. code-block:: bash

   hasura clone <user/my-project>
   cd <my-project>

Now, you'll have the project source code on your computer.

Starter hub projects:
^^^^^^^^^^^^^^^^^^^^^

- `hello-csharp-aspnet <https://hasura.io/hub/project/hasura/hello-csharp-aspnet>`_
- `hello-ghost <https://hasura.io/hub/project/hasura/hello-ghost>`_
- `hello-golang-iris <https://hasura.io/hub/project/hasura/hello-golang-iris>`_
- `hello-java-spark <https://hasura.io/hub/project/hasura/hello-java-spark>`_
- `hello-react-native <https://hasura.io/hub/project/hasura/hello-react-native>`_
- `hello-python-django <https://hasura.io/hub/project/hasura/hello-python-django>`_
- `hello-golang-raw <https://hasura.io/hub/project/hasura/hello-golang-raw>`_
- `hello-python-flask <https://hasura.io/hub/project/hasura/hello-python-flask>`_
- `hello-php-apache <https://hasura.io/hub/project/hasura/hello-php-apache>`_
- `hello-nodejs-express <https://hasura.io/hub/project/hasura/hello-nodejs-express>`_
- `hello-ruby-sinatra <https://hasura.io/hub/project/hasura/hello-ruby-sinatra>`_
- `hello-r-shiny <https://hasura.io/hub/project/hasura/hello-r-shiny>`_
- `hello-android <https://hasura.io/hub/project/hasura/hello-android>`_
- `hello-world <https://hasura.io/hub/project/hasura/hello-world>`_
- `auth-api-quickstart <https://hasura.io/hub/project/hasura/auth-api-quickstart>`_
- `base-python-dash <https://hasura.io/hub/project/hasura/base-python-dash>`_
- `hello-java-springboot <https://hasura.io/hub/project/hasura/hello-java-springboot>`_
- `hello-iOS <https://hasura.io/hub/project/hasura/hello-iOS>`_
- `hello-swift-vapor <https://hasura.io/hub/project/hasura/hello-swift-vapor>`_
- `hello-swift-perfect <https://hasura.io/hub/project/hasura/hello-swift-perfect>`_
- `hello-react <https://hasura.io/hub/project/hasura/hello-react>`_
- `base <https://hasura.io/hub/project/hasura/base>`_
- `hello-angularjs <https://hasura.io/hub/project/hasura/hello-angularjs>`_
- `docker-base <https://hasura.io/hub/project/hasura/docker-base>`_
- `hello-nginx <https://hasura.io/hub/project/hasura/hello-nginx>`_
- `hello-ruby-rails <https://hasura.io/hub/project/hasura/hello-ruby-rails>`_
