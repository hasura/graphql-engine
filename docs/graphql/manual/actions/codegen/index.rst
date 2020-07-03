.. meta::
   :description: Codegen for Hasura actions
   :keywords: hasura, docs, actions, codegen

.. _actions_codegen:

Actions codegen
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Actions need HTTP handlers to run the business logic. It might be inconvenient
to write the complete handler code for every action. Luckily, GraphQL's type
system allows us to auto-generate the boilerplate code for actions.


.. note::

  Hasura currently has codegen set up for a few frameworks. The list of
  supported frameworks should grow with contributions from the
  community.

.. _actions-codegen-execute:

Generating handler code for your action
---------------------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Head to the ``Actions -> [action-name] -> Codegen`` tab in the console

    You can select the framework of your choice to get the corresponding
    handler boilerplate code.

    .. thumbnail:: /img/graphql/manual/actions/console-codegen-tab.png
       :alt: Console codegen tab


  .. tab:: CLI

    **Configuration**

    Before being able to codegen for actions, you have to configure your CLI.

    Run:

    .. code-block:: bash

       hasura actions use-codegen

    1. Choose which framework you want to codegen for:

       .. thumbnail:: /img/graphql/manual/actions/cli-framework-prompt.png
          :alt: CLI Framework Prompt

    2. Choose if you also wish to clone a starter kit for the chosen framework:

       .. thumbnail:: /img/graphql/manual/actions/cli-starter-kit-prompt.png
          :alt: CLI Starter Kit Prompt

    3. Choose a path where you want to output the auto-generated code files

       .. thumbnail:: /img/graphql/manual/actions/cli-output-dir-prompt.png
          :alt: CLI Starter Kit Prompt


    This command will update your ``config.yaml`` with the codegen config as per
    your preferences. You can also set these values manually in ``config.yaml``.

    For example:

    .. code-block:: yaml
       :emphasize-lines: 8-10

        version: "2"
        endpoint: http://localhost:8080
        metadata_directory: metadata
        migrations_directory: migrations
        actions:
          handler_webhook_baseurl: http://localhost:3000
          kind: synchronous
          codegen:
            framework: nodejs-express
            output_dir: ./nodejs-express/src/handlers/



    **Codegen**


    To finally get auto-generated code for an action, run:

    .. code-block:: bash

       hasura actions codegen <action-name>

    The codegen files will be generated at the ``output_dir`` path from ``config.yaml``.


Codegen for your framework
--------------------------

As of now, Hasura provides codegen for a few frameworks (``nodejs-express``,
``typescript-zeit``, etc). 

Refer to this page for examples using codegen:

.. toctree::
  :maxdepth: 1

  Codegen examples <examples/index>

We will continue adding more examples to the documentation.

Building your own codegen
^^^^^^^^^^^^^^^^^^^^^^^^^

If you wish to build a code generator for your framework
`read the contrib guide <https://github.com/hasura/codegen-builder-contrib/>`_.


  