:orphan:

.. meta::
   :description: A guide to getting started with an ASP.Net Core app on Hasura
   :keywords: hasura, guide, csharp, ASP.Net
   :content-tags: getting started, csharp, ASP.Net, .Net, C#

.. title:: Getting started with C# and ASP.Net Core 

.. rst-class:: guide-title

.. rubric:: Getting started with C# and ASP.Net Core

.. role:: charp(code)
   :language: aspx-cs

Introduction
------------

This tutorial will help you build and deploy an ASP.Net Core app using Hasura in minutes.
To get started, you only need ``git`` installed on your computer.

Setup
-----

The first step involves installing and configuring the command line tool ``hasuractl``.
   
#. Install hasuractl by following the instructions here: :ref:`Installing hasuractl <hasuractl-installation>`.

#. Use ``hasuractl`` to create a Hasura project and get a trial: `Creating a project and cluster <hasuractl-getting-started-create-project>`.

#. Initialize a git repo inside your project

   .. code:: bash

      $ git init
   
Deploy a hello-world app
------------------------

In this section, we'll deploy a sample ASP.Net Core app on Hasura.

     If you already have an existing ASP.Net Core app that you wish to deploy on Hasura, you should read through this section to get an idea of how things work, and then check out the next section for instructions on deploying your existing app.

Use the following command to quickly add a sample hello-world app (*let's call it hello-world*) built on ASP.Net Core and set it up for deployment:

.. code:: bash

   # replace <cluster-name> with the name of the cluster you created in the previous section
   $ hasuractl service quickstart hello-world --template csharp-aspnet -c <cluster-name>
   
This command does the following:

* Create a folder called hello-world inside the services directory and initialize it with a sample ASP.Net Core app. You can check out the other files and folders inside it.

* Configure your Hasura cluster to add a service for your app.

* Add a route to your Hasura cluster at which your app will be live (on a sub-domain named after the service, ``hello-world.<cluster-name>.hasura-app.io``).

* Add a git remote for your Hasura cluster so that you can quickly deploy your app/service.

* Adds your SSH key to the cluster to authorize you to push you code to the newly created remote.

* Commits the starter kit code. 

Now deploy the hello-world app in one step using:

.. code:: bash

   $ git push hasura master

This command will push the sample app to a git remote on your Hasura cluster, which then builds a Docker image out of it using the Dockerfile in the services/app-name folder, and deploy it at a subdomain on your cluster.

Now check your app live at `https://app-name.cluster-name.hasura-app.io <`https://app-name.cluster-name.hasura-app.io>`_ !

Deploy an existing app on Hasura
--------------------------------

While we are working on adding more instructions to help you deploy your existing ASP.Net app, please get in touch with us at https://hasura.io/help if you need any help with this.
