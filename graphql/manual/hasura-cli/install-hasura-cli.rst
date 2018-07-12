.. .. meta::
   :description: Installing the hasura CLI on Linux, Mac OS, Windows.
   :keywords: hasura, hasura CLI, install, linux, mac, windows

=========================
Installing the hasura CLI
=========================

Step 1: Install
---------------

.. global-tabs::

   tabs:
     - id: linux
       content: |

         Open your linux shell and run the following command:

         .. code-block:: bash

            curl -L https://cli.hasura.io/install.sh | bash

         This will install the ``hasura`` CLI tool in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: mac
       content: |
         In your terminal enter the following command:

         .. code-block:: bash

            curl -L https://cli.hasura.io/install.sh | bash

         This will install the ``hasura`` CLI in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: windows
       content: |

         .. note::

            You should have ``git bash`` installed to use ``hasura`` CLI. Download git bash using the following `(link) <https://git-scm.com/download/win>`_.
            Also, make sure you install it in ``MinTTY`` mode, instead of Windows' default console.

         Download the ``hasura`` installer:

         * `hasura (64-bit Windows installer) <https://cli.hasura.io/install/windows-amd64>`_
         * `hasura (32-bit Windows installer) <https://cli.hasura.io/install/windows-386>`_
         
         **Note:** Please run the installer as Administrator to avoid PATH update errors. If you're still getting a `command not found` error after installing Hasura, please restart Gitbash.


Step 2: Login
-------------

.. global-tabs::

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
         Next, open ``git-bash`` (or restart ``git-bash`` after installing Hasura CLI) and login/register:

         .. code-block:: bash

            hasura login

         This command will open up the browser and
         allow you to register with a new account (or login to your existing account).


Troubleshooting
^^^^^^^^^^^^^^^

If you face any issue while logging in, checkout the :ref:`Troubleshooting
<hasuractl_alternate_login>` section for an alternate login method. 

(Optional) Add shell completion
-------------------------------

To add command auto completion in the shell

Refer to :ref:`hasura completion <hasura_completion>`
