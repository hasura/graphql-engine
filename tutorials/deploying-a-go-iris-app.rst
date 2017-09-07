:orphan:

.. meta::
   :description: Use a starter kit to deploy your Iris Golang app, with migrations and a PostgreSQL DB, to the cloud using a simple git push command
   :keywords: hasura, docs, tutorials, go, golang, iris, web-application
   :content-tags: go, deployment, web-application
   :created-on: 2017-07-29T12:19:52.475Z
 
Deploying a Iris Golang app
===========================

.. rst-class:: featured-image
.. image:: ../img/hasura-golang.png
   :height: 0px
   :width: 0px

This tutorial will take you over deploying a Iris Golang application on Hasura.

Benefits of using Hasura to deploy and host your Go-Iris app:

1. A Hasura project comes with a pre-configured Postgres that's ready to be used
2. `git push hasura master` inside your go-iris app will deploy your application to your server

Basic deployment
----------------
Follow the 4 steps below so that you can start off and deploy a Iris Golang app
within minutes. Refer to the next section on :ref:`local development`, to connect to develop and test locally.

Step 1a: Get a hasura project and make a note of your credentials
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sign up on http://dashboard.hasura.io and get yourself a Hasura project.
Creating a hasura project will give you a domain. Something like: `project42.hasura-app.io`
You will also get an email with your `admin` credentials for your project console and your
database (search for "hasura credentials" in your inbox if you're having trouble finding the email).

.. code::

   #Project name: project42
   Console link: https://project42.hasura-app.io

   #Postgres
   username: admin
   password: password

Step 1b: Install ``hasuractl``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Install the command line tool: ``hasuractl``.

Read full instructions `here <https://docs.hasura.io/0.14/ref/cli/hasuractl.html>`_.

Once you're done with that, login and setup ``hasuractl``:

.. code-block:: Bash

   # This will pop a browser open, where you should login with your hasura.io account
   $ hasuractl login

Make ``hasuractl`` talk to the specific project you created in Step 1a.
(this was ``project42`` in the example above)

.. code-block:: Bash

   $ hasuractl set-context <project-name>

Step 2: Initialise a Iris Golang project with an app name
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Run the following command to initialise a Go-Iris app that can be instantly deployed:

.. code-block:: Bash

   $ hasuractl quickstart go-iris my-app --create

This is the file structure that will be setup:

.. code-block:: Bash

   ├── my-app
   │   ├── app/ #contains your iris golang project 
   ├── Dockerfile
   ├── glide.lock
   ├── glide.yaml
   ├── .git/
   └── README

Step 3: Use hasuractl to add your SSH key to the Hasura project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can't just start pushing code to a new hasura project without making sure
that you are really you! ``hasuractl`` can push your SSH key to your hasura project cluster
so that you can start pushing your code.

.. code-block:: Bash

   $ hasuractl add-ssh-key

Step 4: ``git push`` and you're done!
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: Bash

   $ git push hasura master

.. _local-development:
Local development
-----------------

Step 1: Setup Glide for managing the vendor directory within a Go package
-------------------------------------------------------------------------

You can use the following command to install glide in order to install vendor packages

.. code::

    $ curl https://glide.sh/get | sh

Step 2: Install vendor packages using glide! 
--------------------------------------------

Now use glide install in your project directory to install vendor packages locally!

.. code::

    $ glide install

Step 3: Build and Run your app locally! 
---------------------------------------

Now use go build -a app/main.go to build and ./main in your project directory to run your app locally!

.. code::

    $ go build -a app/main.go
    $ ./main

Go to ``127.0.0.1:8080`` to see your app live!
