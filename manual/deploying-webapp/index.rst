.. meta::
   :description: Learn how to install Hasura - create a project by claiming a free trial project or by installing Hasura on public cloud infra or on a laptop/desktop.
   :keywords: hasura, installation, cloud, public cloud


.. _project_creation:

Deploying your webapp on Hasura
===============================

Hasura provides a fast and simple way to deploy your app built on your favourite
frameworks as a service on a secure https subdomain. To deploy your code on
Hasura, all you need to do is a ``git push hasura master``!

To set up this simple git push deployment system, you need the following:
- Your app code in a git repository
- A Dockerfile that contains instructions on building a Docker image for your
  app
- A git-push deployment enabled service on Hasura

Quickstart Templates
--------------------

Apps or Services deployed on Hasura run on Docker images, built according to a
Dockerfile. We've prepared `starter kits <https://github.com/hasura/quickstart-docker-git>`_ for all your favourite
frameworks, that already contain pre-configured Dockerfiles for you to quickly
setup your app!

The easiest way to use these templates is to install and set your project
context on Hasuractl as showin in :ref:`hasuractl`, and then do

.. code:

    $ hasuractl quickstart list

This will show you a list of all supported quickstart templates. If your
favourite framework is missing from this list, drop us a message at
support@hasura.io, and we'll get to work adding it.

Choose a template, and use the following command to create your app folder
called <app-name> 

.. code:

    $ hasuractl quickstart <template-name> <app-name> --create

This command will do the following:
- Create a service hosted at <app-name>.<project-name>.hasura-app.io, to which you can deploy your app
- Create a folder called <app-name>, that contains a Dockerfile with instructions
  on building your app
- Copy a hello world app written in the chosen framework into the <app-name>
  directory, which you can later replace with your own app.

Now, cd into the folder, commit your code, and get ready to deploy!

.. code:

    $ cd <app-name>
    $ git commit -am "Initialized"

Make sure to add your ssh-key to your Hasura project - check out
:ref:`add-SSH-keys` for more info.

Now, we deploy our app using

.. code:

    $ git push hasura master

Voila, your service is deployed and live! Check out your service live at <app-name>.<project-name>.hasura-app.io!

In case there are any errors in building or deploying your code, the git push command will show you errors and the push will fail. Fix the error, and push again!

.. admonition:: Behind The Scenes

   The Hasura platform basically builds a docker image from the latest git changes
   pushed by you, and deploys the right kubernetes service, deployment underneath.

   If you want finer control over your deployment, you are encouraged to use ``kubectl``
   and peek under the hood of the service that is automatically deployed.
