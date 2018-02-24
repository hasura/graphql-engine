.. _deploy-webapp:

Hosting your web app or API
============================

Hasura provides a fast and simple way to deploy your app built on your favourite
frameworks as a microservice on a secure ``https`` subdomain. To deploy your code on
Hasura, all you need to do is a ``git push hasura master``!

To set up this simple git push deployment system, you need the following:

* Your app code in a git repository
* A Dockerfile that contains instructions on building a Docker image for your app
* A git-push deployment enabled microservice on Hasura

There are two ways of doing this:

* Use Hasura quickstart apps that have "hello world" for various frameworks
* Use your own Dockerfile


Option 1: Using the Quickstart Apps 
------------------------------------

Apps or Services deployed on Hasura run on Docker images, built according to a
Dockerfile. We've prepared `starter kits <https://hasura.io/hub>`_ for all your
favourite frameworks, that already contain pre-configured Dockerfiles for you
to quickly setup your app!

Create a hasura project
^^^^^^^^^^^^^^^^^^^^^^^
Create a hello-world project from Hasura hub.

.. code-block:: shell

  $ hasura quickstart hello-node-express

This will clone a Hasura project with an existing nodejs-express app, and
create/add a Hasura cluster to the project.

Customize the project
^^^^^^^^^^^^^^^^^^^^^
The ``services`` directory already has a ``node-express`` app with some sample
hello world code, a Dockerfile to build docker image and Kubernetes specs for
deployment in it.

Modify the code in the node-express app, or move your own code into this
directory. You can run your code locally as you normally would.

Add files to git
^^^^^^^^^^^^^^^^
``hasura quickstart`` has already initialised a new git repo in the project
directory. So if we add the files and commit, we can push to deploy.

.. code-block:: console

  $ git add .
  $ git commit -m 'initial commit'

Deploy your app
^^^^^^^^^^^^^^^
Now, we deploy our app using:

.. code-block:: console

    $ git push hasura master

Voila, your microservice is deployed and live! Check out your microservice live at
<app-name>.<cluster-name>.hasura-app.io!

In case there are any errors in building or deploying your code, the git push
command will show you errors and the push will fail. Fix the error, and push
again!

.. admonition:: Behind The Scenes

   The Hasura platform basically builds a docker image from the latest git changes
   pushed by you, and deploys the right kubernetes microservice, deployment underneath.

   If you want finer control over your deployment, you are encouraged to use ``kubectl``
   and peek under the hood of the microservice that is automatically deployed.


Option 2: Using your own Dockerfile (advanced users)
----------------------------------------------------

Make sure you are inside the directory of your Hasura project.


Create a new microservice
^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: shell

  $ hasura microservice add myapp

This will create a new directory in the ``services`` directory with Kubernetes
specs.

Move all your code and Dockerfile into the ``services/myapp`` directory. Change
the ports in the Kubernetes specs according to your code.

Create a route for the microservice
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now to expose the above created microservice, we have to create a route for it.

.. code-block:: shell

  $ hasura route generate myapp

Create a remote for the microservice
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
As we are creating a git-push enabled microservice, we have to add a git remote for
the microservice.

.. code-block:: shell
		
  $ hasura remote generate myapp

**NOTE**: In the ``conf/remotes.yaml`` file make sure the path to your Dockerfile is
correct.

Make sure your SSH key is added
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: shell

  $ hasura ssh-key list

If your SSH key is not there, add it by:

.. code-block:: shell

  $ hasura ssh-key add


Deploying the code
^^^^^^^^^^^^^^^^^^
Now you can commit your changes and push to the hasura
remote to instantly build and deploy your app in one command!

.. code-block:: console

  $ git add .
  $ git commit -m 'sensible commit message'
  $ git push hasura master


Voila, your microservice is deployed and live! Check out your microservice live at
<app-name>.<cluster-name>.hasura-app.io!

In case there are any errors in building or deploying your code, the git push
command will show you errors and the push will fail. Fix the error, and push
again!
