Actions codegen with CLI
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Configuration
-------------

Before getting the codegen, you have to configure your codegen. Run:

.. code-block:: bash

    hasura actions use-codegen

1. Choose which framework you want to codegen for:

.. thumbnail:: ../../../../img/graphql/manual/actions/codegen/cli-framework-prompt.png
   :alt: CLI Framework Prompt

2. Choose if you also wish to clone a starter kit:

.. thumbnail:: ../../../../img/graphql/manual/actions/codegen/cli-starter-kit-prompt.png
   :alt: CLI Starter Kit Prompt

3. Choose a path where you want to output the codegen files

.. thumbnail:: ../../../../img/graphql/manual/actions/codegen/cli-output-dir-prompt.png
   :alt: CLI Starter Kit Prompt


This command will update your ``config.yaml`` with the codegen config as per your preferences. You can also set these values manually in ``config.yaml``. For example:

.. code-block:: yaml

    actions:
      codegen:
        framework: nodejs-express
        output_dir: ./nodejs-express/src/handlers/
      handler_webhook_baseurl: http://localhost:3000
      kind: synchronous
    endpoint: http://localhost:8080
    metadata_directory: metadata
    migrations_directory: migrations
    version: "2"


Codegen
-------

To finally get codegen for an action, run:

.. code-block:: bash

    hasura actions codegen <action-name>

The codegen files will be generated at the ``output_dir`` path from ``config.yaml``.