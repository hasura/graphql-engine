.. meta::
   :description: Hasura Pro CLI
   :keywords: hasura, docs, command line interface, cli

.. _hasurapro_cli:

Hasura Pro CLI
==============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


Installing the Hasura Pro CLI
-----------------------------

Hasura Pro CLI is distributed as a plugin to the :ref:`Hasura Core CLI <hasuracli_manual>`

- Follow the instructions :ref:`here <install_hasura_cli>` to install Hasura Core CLI

- Then execute the following command to install the Hasura Pro CLI plugin:

  .. code-block:: bash

    hasura plugins install pro

- You can verify the installation by executing the ``help`` command:

  .. code-block:: bash

     hasura pro --help

Authentication with the Hasura Pro CLI
--------------------------------------

All interactions from the CLI to Hasura’s APIs are authenticated using a personal access token generated for your user account.

To set up a token, execute the following command on the CLI:

.. code-block:: bash

   hasura pro login

This command will show a prompt for entering the personal access token.

Head to your `Hasura Cloud account settings <https://cloud.hasura.io>`_ to create a new token. You can name it something like "cli". Note that the token will be shown only once and as soon as you copy the token, paste it in your terminal prompt.

.. admonition:: Keep this token secure!

  This token can be used to authenticate against Hasura Pro APIs and your Hasura Cloud projects. Make sure you keep it secure. This is a one-time operation. The token will be valid until you delete it.

Upgrading the CLI
-----------------

To upgrade to a newer version, you can use the ``upgrade`` command:

.. code-block:: bash

   hasura plugins upgrade pro

Uninstalling the CLI
--------------------

To uninstall the CLI, use the ``uninstall`` command:

.. code-block:: bash

   hasura plugins uninstall pro