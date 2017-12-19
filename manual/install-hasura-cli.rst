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

         **Note:** You should have ``git bash`` installed to use ``hasura`` CLI. Download git bash using the following `(link) <https://git-scm.com/download/win>`_

         Download the ``hasura`` installer:

         * `hasura (64-bit Windows installer) <https://hasura.io/install.msi>`_
         * `hasura (32-bit Windows installer) <https://storage.googleapis.com/hasuractl/stable/windows-386/hasura.msi>`_


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

Adding shell completion (optional)
----------------------------------

Refer to :ref:`hasura completion <hasura_completion>`
