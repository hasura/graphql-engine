.. meta::
   :description: Part 1 of a set of learning exercises meant for exploring Hasura in detail. This pre-requisite part deals with creating a Hasura project.
   :keywords: hasura, getting started, step 1

============================
Part I: Setup the Hasura CLI
============================

The ``hasura`` CLI is a command line utility to help you get your backend setup quickly. It helps you create projects, manage clusters and manage microservices and explore APIs running on the cluster.

.. admonition:: Note

   ``hasura`` works using a CLI experience only so that you can mantain all the files that contain your configuration,
   source code and database schema information on your filesystem directly. This makes it easy for you to collaborate using
   tools like git.

Step 1: Install the hasura CLI tool
-----------------------------------

.. tabs::

   tabs:
     - id: linux
       content: |
         Open your linux shell and run the following command:

         .. code-block:: bash

            curl -L https://storage.googleapis.com/hasuractl/install-stg.sh | bash

         This will install the ``hasura`` CLI tool in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: mac
       content: |
         In your terminal enter the following command:

         .. code-block:: bash

            curl -L https://storage.googleapis.com/hasuractl/install-stg.sh | bash

         This will install the ``hasura`` CLI in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: windows
       content: |

         Note: You need to be running 64-bit windows to run the ``hasura`` CLI.
         Download the ``hasura`` installer from here: `hasura (Windows installer) <https://storage.googleapis.com/hasuractl/v0.2.1/windows-amd64/hasura.msi>`_


Step 2: Login
-------------

Next, run the command below to login or create a new Hasura account.

.. code-block:: console

  $ hasura login

A browser window will open up for you to login or register.
Once you've signed in, you'll see a 'Login success' message on the browser.
Close the browser tab, and return to the terminal.

You have now created an account or logged in to your account on hasura.io.
You can also view your account profile and other details on
`dashboard.hasura.io <https://dashboard.hasura.io>`_. You will NOT need to use the 'dashboard'
for the remainder of this tutorial and you can ignore it for now.

Next: Create a Hasura project
-----------------------------

Next, let's head to :doc:`Part II: Create a Hasura project<2-hasura-project>`.
