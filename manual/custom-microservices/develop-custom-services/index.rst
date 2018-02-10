.. meta::
   :description: Developing and Hosting webapps
   :keywords: hasura, manual, hosting, webapp, custom microservice

.. _deploy-host-webapps:

==========================
Developing webapps or APIs
==========================

Adding a microservice to an existing Hasura project
---------------------------------------------------

Hasura provides a very simple and powerful system to develop, deploy and host your custom webapps easily. This is facilitated by the `Hasura Hub <https://hasura.io/hub>`_, a user contributed repository of working projects that are just one command away from a running app.

The Hasura Hub contains several quickstart projects that can be used to bootstrap your new app and quickly get a simple Hello World app up and running on Hasura using your favourite framework.

Setup
^^^^^

Once the hasura cli is installed, you can get started with your favourite framework by using the quickstart command.

Let's start with a `python-flask quickstart <https://hasura.io/hub/project/hasura/hello-python-flask>`_. This quickstart project comes with the following by default:

1. A basic Hasura project

2. Two tables ``article`` and ``author`` with some dummy data

3. A basic flask app which runs at the ``app`` subdomain which fetches a list of articles available at the */get_articles* endpoint.

.. code:: bash

    $ hasura quickstart hello-python-flask

This command will do the following:

1. Create a Hasura free-trial cluster

2. Clone the ``hello-python-flask`` project from the Hasura Hub

3. Add the above created cluster to the project and set it as the default cluster

4. Initialize a git repo inside the project directory
   
5. Add you SSH key to the cluster
   

Getting cluster information
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Every hasura project is run on a Hasura cluster. To get details about the cluster this project is running on:

.. code:: bash

    $ hasura cluster status

This will give you cluster status, which looks like:

.. code:: bash

          INFO Status:
          Cluster Name:       excise98
          Cluster Alias:      hasura
          Kube Context:       excise98
          Platform Version:   v0.15.3
          Cluster State:      Synced

Keep a note of your cluster name. Alternatively, you can also go to your `Hasura dashboard <http://dashboard.hasura.io>`_ and see the clusters you have.

Deploying on a Hasura cluster
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To deploy your app:

.. code:: bash

    $ git add .
    $ git commit -m "Initial Commit"
    $ git push hasura master

Once the above commands are executed successfully, head over to ``https://app.cluster-name.hasura-app.io`` (in this case, ``https://app.excise98.hasura-app.io``) to view your flask app.

API Console
"""""""""""

Every hasura cluster comes with an API Console that gives you a GUI to test out the BaaS features of Hasura. To open the API Console for your default cluster:

.. code:: bash

    $ hasura api-console

You can use the API console to check and modify your Postgres schema and database. On making modifications to your schema (your database structure), the API Console will create migration files in your migration folder, and apply them on your cluster.
To learn more about migrations, check out :doc:`the schema migration docs <../../data/data-migration>`.

Using a Dockerfile
------------------

Microservices on Hasura are deployed as Docker containers managed on a Kubernetes cluster. A normal microservice on Hasura consists of the following:

1. A ``Dockerfile`` - this contains the instructions for building the Docker image
2. A ``k8s.yaml`` file that contains all the kubernetes configuration required to manage the Docker image (By default, a service and a deployment)
3. A source folder (named after the microservice name) in the ``microservices`` directory that contains the above ``Dockerfile`` and ``k8s.yaml`` files and your source code.

To add your own custom microservice to your Hasura project, start by adding a microservice:

.. code:: bash

    $ hasura microservice generate my-service

(Make sure you add a ``-c cluster-name`` if you didn't set your default cluster using ``hasura cluster set-default -c cluster-alias``. By default, quickstart aliases the cluster it creates to ``hasura`` and sets it as the default, so you won't need to do this.)

This command will do the following:

1. Create a ``my-service`` directory inside the ``microservices`` directory.
2. Create a ``k8s.yaml`` file inside the ``my-service`` directory.

Note that this command does not actually make any changes to your cluster, it just generates files on your local Hasura project directory.

To apply this configuration, and create the microservice on your cluster ``git commit`` and ``git push`` the project directory:

.. code:: bash

    $ git commit -m "added my-service"
    $ git push hasura master

This command will automatically pick up the microservice configuration from the ``microservices`` directory and apply them on the default cluster. ( This command will also update the configuration if you run it after making changes to the ``k8s.yaml`` file. )

Now the cluster should have a microservice called ``my-service`` running on it. You can check this again using:

.. code:: bash

    $ hasura cluster status

This should show a microservice called ``my-service`` running under the ``Custom microservices`` section. The ``URL`` column will be empty, since we haven't configured a route for your microservice yet.

.. code:: bash

    INFO Custom microservices:
    NAME   STATUS    URL
    my-service    Running

The routes for all the microservices on your Hasura project are configured in ``conf/routes.yaml``. The ``hasura`` CLI provides a handy command to generate the default routes configuration for your custom microservice:

.. code:: bash

    $ hasura conf generate-routes my-service >> conf/routes.yaml

This command will add the default route configuration for your microservice to the ``conf/routes.yaml`` file.

Once you've added a route, you should also add a remote, so that you can use git push to deploy your microservice. As with the routes, the remotes are configured in the ``conf/ci.yaml``. You can use the following hasura cli command to generate the default remote configuration:

.. code:: bash

    $ hasura conf generate-remotes my-service >> conf/ci.yaml

Once you add the route and remote configuration, apply the changes using ``git commit`` and ``git push``:

.. code:: bash

    $ git commit -m "added route and remote for my-service"
    $ git push hasura master

This will add a route and a remote to your microservice, letting you access the app at a ``my-service.cluster-name.hasura-app.io`` (where ``cluster-name`` is the cluster-name from the ``hasura cluster status`` command), and also adds the remote configuration that builds and deploys your microservice when you do a ``git push`` to the cluster remote.

So now the cluster status will show:

.. code:: bash

    INFO Custom microservices:
    NAME          STATUS    URL
    my-service    Running   https://my-service.cluster-name.hasura-app.io

This means that your custom microservice will be available at the url ``https://my-service.cluster-name.hasura-app.io``. Visiting this url now will show you a "Hello World!" message.

Running your app on other than port 8080
----------------------------------------

Your microservices on hasura cluster runs on port 8080 by default. To run your app on hasura cluster, you can do the followings.

1. Create new microservice

You can simply run the command while generating a new microservice:

.. code:: bash

    $ hasura microservice create <name-of-ms>  --port <port number>

2. For Existing microservice running on port 8080

If you already have an app on a existing microservice ( which is running on port 8080), you have to  assign values of containerPort and targetPort to <port-number> in your ``/microservice/<microservice-name>/k8s.yaml`` file:

``containerPort: <port-number>``

``targetPort: <port-number>``

To apply this configuration on your cluster ``git commit`` and ``git push`` the project directory:

.. code:: bash

    $ git commit -m "<microservice-name> port changed"
    $ git push hasura master


Contacting internal URLs on microservices
-----------------------------------------

The Hasura BaaS APIs can be contacted through two URLs, or endpoints.

1. The external URL (external endpoint) - this is of the form ``https://service-name.cluster-name.hasura-app.io``

This is a https url, protected by ssl certificates that Hasura generates through LetsEncrypt. The authentication for this is handled by the gateway, which converts the Authorization token sent along with the query into two headers, the ``X-Hasura-User-Id`` and the ``X-Hasura-Roles``. These two Headers are used by Hasura to manage session. Check out the documentation on ``Session Middleware`` for more information!
This URL can be used to contact the service from anywhere on the internet.
You can check this using ``hasura microservice status``

.. code:: bash

   $ hasura cluster status

2. The internal URL (internal endpoint) - this is of the form ``http://service-name.namespace``

This internal URL is the URL that microservices running on the same Hasura cluster can use to contact the microservice.
The Hasura microservices are all in the ``hasura`` namespace, and all custom microservices are in the ``default`` namespace.
Since the session management is handled by Hasura, authentication for queries to the internal URL can be done by adding two headers, the ``X-Hasura-User-Id``, which is the user id of the user you want to run the query as, and ``X-Hasura-Roles``, which is an array of the roles that you want to run the query as. Check out the documentation on the **Session Middleware** for more information!

Using Session Middleware
------------------------

The Hasura session middleware resides in the Gateway microservice, and handles session management for the entire platform. Every query made to an external URL on the Hasura app goes through the Gateway microservice, which looks for an Authentication header in the query. Based on the token in the Authentication header, the Gateway microservice will lookup the session details for the user and replace the header with two other headers - X-Hasura-User-Id, which contains the user id of the user logging in (as per the auth user database), and X-Hasura-Roles, which contains a list of roles the user is assigned.

Microservices running on Hasura can directly look for these headers, and permit access or process the user based on the content of these headers.
For more info, check out the documentation on the Hasura ``Session Middleware``!

Get Logs
--------

To get logs for your microservice, you can use the ``hasura logs`` command:

.. code:: bash

   $  hasura microservice logs my-service

The ``-n`` flag is the namespace in which the service resides. All Hasura
microservices are in the ``hasura`` namespace.
