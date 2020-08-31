.. meta::
   :description: Visual studio code integration with Hasura
   :keywords: hasura, docs, guide, code editor, integration, visual studio code, vs code

.. _guides_vs_code:

Guides: Visual Studio Code Setup
================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:


If you use `Visual Studio code <https://code.visualstudio.com/>`__, the `Apollo GraphQL plugin <https://marketplace.visualstudio.com/items?itemName=apollographql.vscode-apollo>`__ can improve your development experience significantly by enabling a lot of cool features like syntax highlighting for GraphQL, auto completion for GraphQL requests and validating your GraphQL requests against a schema or an endpoint.

This guide helps you configure the Apollo GraphQL plugin with Hasura to make your local development easier.

Install the plugin
------------------

Launch VS Code Quick Open (Ctrl+P), paste the following command, and press enter.

.. code-block:: bash
  
  ext install apollographql.vscode-apollo

Configure your project
----------------------

Create a file called ``apollo.config.js`` in the root of your project and add the following content:

.. code-block:: javascript

    module.exports = {
      client: {
        service: {
          name: "your-service-name",
          url: "http://localhost:8080/v1/graphql",
          headers: {
            "x-hasura-admin-secret": "<your-admin-secret>"
          }
        }
      }
    };

Notes:

- Replace ``http://localhost:8080/v1/graphql`` with your GraphQL endpoint.
- You can also add custom headers in the headers object if you wish to emulate the schema for some specific roles or tokens.

For advanced configuration, check out the `docs for the plugin <https://marketplace.visualstudio.com/items?itemName=apollographql.vscode-apollo>`__.

Note: The `VSCode GraphQL <https://github.com/prisma/vscode-graphql>`__ plugin by Prisma does not currently work with Hasura because it has a hard dependency on batching and Hasura does not support batching as of now. Batching as a feature in GraphQL engine is tracked `here <https://github.com/hasura/graphql-engine/issues/1812>`__.
