.. .. meta::
   :description: Installing the hasura CLI on Linux, Mac OS, Windows.
   :keywords: hasura, hasura CLI, install, linux, mac, windows

=========================
Installing the hasura CLI
=========================

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

         **Note:** You should be running 64-bit windows, and should have ``git`` installed to run the ``hasura`` CLI.

         Download the ``hasura`` installer from here: `hasura (Windows installer) <https://hasura.io/install.msi>`_


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
         Next, open the command prompt, or ``git-bash`` and login/register:

         .. code-block:: bash

            hasura.exe login

         This command will open up the browser and
         allow you to register with a new account (or login to your existing account).
