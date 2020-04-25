.. meta::
   :description: Install the Hasura CLI on Linux, Mac OS, Windows
   :keywords: hasura, hasura cli, install, linux, mac, windows

.. _install_hasura_cli:

Installing the Hasura CLI
=========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Install a binary globally
-------------------------

.. global-tabs::

   tabs:
     - id: linux
       content: |

         Open your linux shell and run the following command:

         .. code-block:: bash

            curl -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | bash

         This will install the Hasura CLI in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

         If you'd prefer to install to a different location other than ``/usr/local/bin``, set the
         env var ``INSTALL_PATH``:

         .. code-block:: bash

            curl -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | INSTALL_PATH=$HOME/bin bash



     - id: mac
       content: |
         In your terminal enter the following command:

         .. code-block:: bash

            curl -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | bash

         This will install the Hasura CLI in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

         If you'd prefer to install to a different location other than ``/usr/local/bin``, set the
         env var ``INSTALL_PATH``:

         .. code-block:: bash

            curl -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | INSTALL_PATH=$HOME/bin bash

     - id: windows
       content: |

         Download the binary ``cli-hasura-windows-amd64.exe`` available under ``Assets`` of the latest release
         from the GitHub release page: https://github.com/hasura/graphql-engine/releases

         Rename the downloaded file to ``hasura``. 
         You can add the path to the environment variable ``PATH`` for making ``hasura`` accessible globally.

Install through npm
-------------------

Hasura CLI is available as an npm package that is independently maintained by some members of the community.
It can be beneficial to use the npm package if you want a version-fixed cli dedicated to your node.js project.
You can find usage details(e.g. script flag tips) in  the `original repository <https://github.com/jjangga0214/hasura-cli>`_.

.. code-block:: bash
   
   # install as a devDependency of your project
   npm install --save-dev hasura-cli[@tag|@version]
   
   # or install globally on your system
   npm install --global hasura-cli[@tag|@version]

(Optional) Add shell completion
-------------------------------

To add command auto completion in the shell, refer to :ref:`hasura completion <hasura_completion>`.
