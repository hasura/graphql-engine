.. meta::
   :description: Uninstall the Hasura CLI on Linux, Mac OS, Windows
   :keywords: hasura, hasura CLI, uninstall, linux, mac, windows

.. _uninstall_hasura_cli:

Uninstalling the Hasura CLI
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Uninstall a binary
------------------

If you installed the binary directly on your system (probably through shell script), 
delete the binary file from its installation location.

.. code-block:: bash

  # By default, the binary is installed at /usr/local/bin/hasura
  $ which hasura
    /usr/local/bin/hasura

  # use sudo if required
  $ rm /usr/local/bin/hasura


Uninstall an npm package
------------------------

If you installed the CLI as an npm package, use npm (or yarn).

.. code-block:: bash

   # if installed as a project dependency
   npm uninstall hasura-cli

   # if installed as a global package
   npm uninstall --global hasura-cli
