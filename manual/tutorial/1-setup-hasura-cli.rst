.. .. meta::
   :description: Part 1 of a set of learning exercises meant for exploring Hasura in detail. This pre-requisite part deals with creating a Hasura project.
   :keywords: hasura, getting started, step 1

============================
Part I: Setup the Hasura CLI
============================

The ``hasura`` CLI is a command line utility to help you get your backend setup quickly. It helps you create projects, manage clusters and manage microservices and explore APIs running on the cluster.

.. admonition:: Note

   ``hasura`` works using a CLI experience only so that you can maintain all the files that contain your configuration,
   source code and database schema information on your filesystem directly. This makes it easy for you to collaborate using
   tools like git.

Step 1: Install
---------------

.. tabs::

   tabs:
     - id: linux
       content: |

         Open your linux shell and run the following command:

         .. code-block:: bash

            curl -L https://hasura.io/install.sh | bash

         This will install the ``hasura`` CLI tool in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: mac
       content: |
         In your terminal enter the following command:

         .. code-block:: bash

            curl -L https://hasura.io/install.sh | bash

         This will install the ``hasura`` CLI in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: windows
       content: |

         **Note:** You should have ``git bash`` installed to use ``hasura`` CLI. Download git bash using the following `(link) <https://git-scm.com/download/win>`_

         Download the ``hasura`` installer:

         * `hasura (64-bit Windows installer) <https://hasura.io/install/windows-amd64>`_
         * `hasura (32-bit Windows installer) <https://hasura.io/install/windows-386>`_
         
         **Note:** Please run the installer as Administrator to avoid PATH update errors. If you're still getting a `command not found` error after installing Hasura, please restart Gitbash.

Step 2: Login
-------------

.. tabs::

   tabs:
     - id: linux
       content: |

         Next, login or register by running the following command:

         .. code-block:: bash

            hasura login

         This command will open up the browser and
         allow you to register with a new account (or login to your existing account).

     - id: mac
       content: |
         Next, login or register by running the following command:

         .. code-block:: bash

            hasura login

         This command will open up the browser and
         allow you to register with a new account (or login to your existing account).

     - id: windows
       content: |
         Next, open ``git-bash`` and login/register:

         .. code-block:: bash

            hasura login

         This command will open up the browser and
         allow you to register with a new account (or login to your existing account).

Next: Create a Hasura project
-----------------------------

Next, let's head to :doc:`Part II: Create a Hasura project<2-hasura-project>`.

..
     - id: windows
       content: |
         **Note:** You should be running 64-bit windows, and should have ``git`` installed to run the ``hasura`` CLI.
         Download the ``hasura`` installer from here: `hasura (Windows installer) <https://storage.googleapis.com/hasuractl/stable/windows-amd64/hasura.msi>`_
     - id: windows
       content: |
         Next, open the command prompt, or ``git-bash`` and login/register:
         .. code-block:: bash
            hasura.exe login
         This command will open up the browser and
         allow you to register with a new account (or login to your existing account).

