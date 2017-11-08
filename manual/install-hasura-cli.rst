.. .. meta::
   :description: Installing the hasura CLI on Linux, Mac OS, Windows.
   :keywords: hasura, hasura CLI, install, linux, mac, windows

=========================
Installing the hasura CLI
=========================

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

         **Note:** You should be running 64-bit windows to run the ``hasura`` CLI.

         Download the ``hasura`` installer from here: `hasura (Windows installer) <https://storage.googleapis.com/hasuractl/v0.2.1/windows-amd64/hasura.msi>`_

