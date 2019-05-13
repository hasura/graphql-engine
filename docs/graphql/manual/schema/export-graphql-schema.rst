Exporting the Hasura GraphQL schema
===================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

If you need to share, introspect or export the GraphQL schema, you can use community tooling such as
`graphqurl <https://github.com/hasura/graphqurl>`__, `Apollo CLI <https://github.com/apollographql/apollo-tooling>`__,
`get-graphql-schema <https://github.com/prismagraphql/get-graphql-schema>`__, etc.

Using **graphqurl**
-------------------

For example, using ``graphqurl``, you can get the schema as follows:

Run ``npm install -g graphqurl`` to install ``graphqurl``. Then you can run the following commands to download the
GraphQL schema:

.. code-block:: bash

  # If the GraphQL engine is running at https://my-graphql-engine.com/v1/graphql,
  # without an admin secret
  gq https://my-graphql-engine.com/v1/graphql --introspect > schema.graphql

  # If Hasura GraphQL Engine is running with an admin secret
  gq https://my-graphql-engine.com/v1/graphql -H 'X-Hasura-Admin-Secret: adminsecretkey' --introspect > schema.graphql

By default, it downloads the schema in ``.graphql`` format. If you want it in JSON format, you can use an additional
flag ``--format json``:

.. code-block:: bash

  # Getting the schema in .json format
  gq https://my-graphql-engine.com/v1/graphql --introspect --format json > schema.json

Using **Apollo CLI**
--------------------

Using Apollo CLI, you can get the schema as follows:

Run ``npm install -g apollo`` to install the Apollo CLI. You can then run the following command to download the GraphQL schema:

.. code-block:: bash

  # If the GraphQL engine is running at https://my-graphql-engine.com/v1/graphql,
  # without an admin secret
  apollo schema:download --endpoint https://my-graphql-engine.com/v1/graphql

  # If Hasura GraphQL Engine is running with an admin secret
  apollo schema:download --endpoint https://my-graphql-engine.com/v1/graphql --header 'X-Hasura-Admin-Secret: adminsecretkey'

Note that ``apollo schema:download`` is an alias of the command `apollo service:download <https://github.com/apollographql/apollo-tooling#apollo-servicedownload-output>`__.

By default, this downloads the schema to a file called ``schema.json``. This command has no other output types.
