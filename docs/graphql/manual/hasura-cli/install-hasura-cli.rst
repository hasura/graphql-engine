.. .. meta::
   :description: Installing the hasura CLI on Linux, Mac OS, Windows.
   :keywords: hasura, hasura CLI, install, linux, mac, windows

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

            curl -L https://cli.hasura.io/install.sh | bash

         This will install the Hasura CLI in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: mac
       content: |
         In your terminal enter the following command:

         .. code-block:: bash

            curl -L https://cli.hasura.io/install.sh | bash

         This will install the Hasura CLI in ``/usr/local/bin``. You might have to provide
         your ``sudo`` password depending on the permissions of your ``/usr/local/bin`` location.

     - id: windows
       content: |

         .. note::

            You should have ``git bash`` installed to use Hasura CLI. Download ``git bash`` using this
            `link <https://git-scm.com/download/win>`_. Also, make sure you install it in ``MinTTY`` mode,
            instead of Windows' default console.

         Download the Hasura installer:

         * `hasura (64-bit Windows installer) <https://cli.hasura.io/install/windows-amd64>`_
         * `hasura (32-bit Windows installer) <https://cli.hasura.io/install/windows-386>`_
         
         **Note:** Please run the installer as ``Administrator`` to avoid PATH update errors. If you're still getting
         a "command not found" error after installing Hasura CLI, please restart ``git bash``.

Alternatively, you can directly download the appropriate binary from: https://github.com/hasura/graphql-engine/releases

(Optional) Add shell completion
-------------------------------

To add command auto completion in the shell

Refer to :ref:`hasura completion <hasura_completion>`
