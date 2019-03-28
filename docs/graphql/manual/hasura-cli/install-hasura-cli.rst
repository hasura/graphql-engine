.. .. meta::
   :description: Installing the hasura CLI on Linux, Mac OS, Windows.
   :keywords: hasura, hasura CLI, install, linux, mac, windows

.. _install_hasura_cli:

Installing the Hasura CLI
=========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Install
-------

.. global-tabs::

   tabs:
     - id: linux
       content: |

         Open your linux shell and run the following command:

         .. code-block:: bash

            curl -L https://github.com/hasura/graphql-engine/raw/master/cli/get.sh | bash

         This will install the Hasura CLI in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

         If you'd prefer to install to a different location other than ``/usr/local/bin``, set the
         env var ``INSTALL_PATH``:

         .. code-block:: bash

            curl -L https://github.com/hasura/graphql-engine/raw/master/cli/get.sh | INSTALL_PATH=$HOME/bin bash



     - id: mac
       content: |
         In your terminal enter the following command:

         .. code-block:: bash

            curl -L https://github.com/hasura/graphql-engine/raw/master/cli/get.sh | bash

         This will install the Hasura CLI in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

         If you'd prefer to install to a different location other than ``/usr/local/bin``, set the
         env var ``INSTALL_PATH``:

         .. code-block:: bash

            curl -L https://github.com/hasura/graphql-engine/raw/master/cli/get.sh | INSTALL_PATH=$HOME/bin bash

     - id: windows
       content: |

         Download the binary ``cli-hasura-windows-amd64.exe`` from GitHub release page: https://github.com/hasura/graphql-engine/releases

         Rename the downloaded file to ``hasura``.

(Optional) Add shell completion
-------------------------------

To add command auto completion in the shell

Refer to :ref:`hasura completion <hasura_completion>`
