Exporting the Hasura GraphQL schema
===================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

If you need to share, introspect or export the GraphQL schema, you can use community tooling such as
`graphqurl <https://github.com/hasura/graphqurl>`_, `apollo <https://github.com/apollographql/apollo-cli>`_,
`get-graphql-schema <https://github.com/prismagraphql/get-graphql-schema>`_, etc.

Using **graphqurl**
-------------------

For example, using ``graphqurl``, you can get the schema as follows:

Run ``npm install -g graphqurl`` to install ``graphqurl``. Then you can run the following commands to download the
GraphQL schema:

.. code-block:: bash

  # If the GraphQL engine is running at https://my-graphql-engine.com/v1alpha1/graphql,
  # without an access key
  gq https://my-graphql-engine.com/v1alpha1/graphql --introspect > schema.graphql

  # If Hasura GraphQL Engine is running with an access key
  gq https://my-graphql-engine.com/v1alpha1/graphql -H 'X-Hasura-Access-Key: secretaccesskey' --introspect > schema.graphql

By default, it downloads the schema in ``.graphql`` format. If you want it in JSON format, you can use an additional
flag ``--format json``:

.. code-block:: bash

  # Getting the schema in .json format
  gq https://my-graphql-engine.com/v1alpha1/graphql --introspect --format json > schema.json
