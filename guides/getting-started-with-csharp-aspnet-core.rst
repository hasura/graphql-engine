:orphan:

.. meta::
   :description: A guide to getting started with an ASP.Net Core app on Hasura
   :keywords: hasura, guide, csharp, ASP.Net
   :content-tags: getting started, csharp, ASP.Net, .Net, C#

.. title:: Deploy an ASP.Net Core (C#) application on Hasura

.. rst-class:: guide-title

.. rubric:: Deploy an ASP.Net Core app

.. role:: charp(code)
   :language: aspx-cs

Introduction
------------

This tutorial will help you build and deploy an ASP.Net Core app using Hasura in minutes.
To get started, you only need ``git`` installed on your computer.

That's it! You can now go ahead and get started with the next section!


Setup
-----

The first step involves installing and configuring the command line tool ``hasuractl``.
   
Install hasuractl on your favourite operating system:

Installation
^^^^^^^^^^^^

Linux
~~~~~

Install the latest ``hasuractl`` using the following command:

.. code:: bash

    $ curl -Lo hasuractl https://storage.googleapis.com/hasuractl/latest/linux-amd64/hasuractl && chmod +x hasuractl && sudo mv hasuractl /usr/local/bin/

If you would like to add hasuractl manually to your path drop the ``sudo mv hasuractl /usr/local/bin`` from the above command

Windows
~~~~~~~

Download `hasuractl.exe <https://storage.googleapis.com/hasuractl/latest/windows-amd64/hasuractl.exe>`_.
and place it in your ``PATH``. Refer to this `video <https://drive.google.com/file/d/0B_G1GgYOqazYUDJFcVhmNHE1UnM/view>`_
if you need help with the installation on Windows.

    In Windows, you should only use ``git-bash`` to execute commands that you see in this documentation.

Mac OS
~~~~~~

Run the following command to install ``hasuractl``:

.. code:: bash

    $ curl -Lo hasuractl https://storage.googleapis.com/hasuractl/latest/darwin-amd64/hasuractl && chmod +x hasuractl && sudo mv hasuractl /usr/local/bin/

If you would like to add hasuractl manually to your path drop the ``sudo mv hasuractl /usr/local/bin`` from the above command

Post Installation
^^^^^^^^^^^^^^^^^

Once ``hasuractl`` is installed, you will need to login to Hasura using:

.. code:: bash

   $ hasuractl login

This command will take you to your browser to perform login and authorization and return you once you are done.

Next, you can create a directory for your Hasura project using:

.. code:: bash

   $ mkdir project-name && cd project-name

Once you're in the directory you want to use for your project, create a free Hasura trial project using

.. code:: bash

   $ hasuractl init --type=trial

Make a note of the cluster name it displays at the end of the command, say cluster-name.

This command will

* Create the following directories in your project folder:

  .. code::

     .
     ├── clusters
     ├── hasura.yaml
     ├── migrations
     └── services



* Create a free Hasura trial cluster, the name of the cluster is printed out at the end

* Add the trial cluster to the clusters folder in your project-name directory.

Once this is done, you can open the Hasura console with:

.. code:: bash

   $ hasuractl api-console -c cluster-name

Explore the console, and try out the various Hasura APIs at the API Explorer!

Before moving on, let's initialize a git repository in our project folder in order to maintain version control, and to easily deploy using git push.

.. code:: bash

   $ git init

When you're ready to deploy your app, move on to the next section.

Deploy a hello-world app
------------------------

In this section, we'll deploy a sample ASP.Net Core app on Hasura.

     If you already have an existing ASP.Net Core project that you wish to deploy on Hasura, you can read through this section to get an idea of what happens, and then check out the next section for instructions on deploying your app.

Use the following command to quickly add a sample hello-world app (*let's call it app-name*) built on ASP.Net Core and set it up for deployment:

.. code:: bash

   $ hasuractl service quickstart app-name --template csharp-aspnet -c cluster-name

This command will do the following:

* Create a folder called app-name inside the services directory and initialize it with a sample ASP.Net Core app. You can check out the other files and folders inside it.

* Configure your Hasura cluster to add a service for your app

* Add a route to your Hasura project at which your app will be live

* Add a git remote to you Hasura project so that you can quickly deploy your project

Once you have the quickstart directory ready, you should add and commit your code to get ready for deploying:

.. code:: bash

   $ git add . && git commit -m "Initialized hello world"

Now deploy your sample app in one step using

.. code:: bash

   $ git push hasura master

This command will push your sample app to a git remote on your Hasura cluster, which then builds a Docker image out of it using the Dockerfile in the services/app-name folder, and deploy it at a subdomain on your cluster.

Now check your app live at `https://app-name.cluster-name.hasura-app.io <`https://app-name.cluster-name.hasura-app.io>`_ !

Deploy an existing app on Hasura
--------------------------------

While we are working on adding more instructions to help you deploy your existing ASP.Net app, please get in touch with us at https://hasura.io/help if you need any help with this.
